namespace KDLFSharp.Core

open System
open System.Globalization
open KDLFSharp.Core.IR

/// Helpers for converting between the parsed AST representation and the IR used for JSON/XML interop.
module DocumentConversion =
    let private invariant = CultureInfo.InvariantCulture

    let private stripNumericSeparators (text: string) =
        text.Replace("_", "", StringComparison.Ordinal)

    let private parseDecimal (raw: string) =
        match Double.TryParse(raw, NumberStyles.Float, invariant) with
        | true, value -> Ok value
        | _ -> Error $"Unable to parse decimal number: {raw}"

    let private parseSignedWithBase (raw: string) (prefixLength: int) (radix: int) =
        let normalized = stripNumericSeparators raw

        if normalized.Length <= prefixLength then
            Error $"Invalid number literal: {raw}"
        else
            let sign, digits =
                if normalized.StartsWith("-", StringComparison.Ordinal) then
                    -1.0, normalized.Substring(1 + prefixLength)
                elif normalized.StartsWith("+", StringComparison.Ordinal) then
                    1.0, normalized.Substring(1 + prefixLength)
                else
                    1.0, normalized.Substring(prefixLength)

            try
                let magnitude = Convert.ToInt64(digits, radix) |> float
                Ok(sign * magnitude)
            with _ ->
                Error $"Unable to parse base-{radix} number: {raw}"

    let private parseNumberLiteral (literal: NumberLiteral) =
        match literal.Kind with
        | NumberKind.Decimal -> literal.Raw |> stripNumericSeparators |> parseDecimal
        | NumberKind.Hexadecimal -> parseSignedWithBase literal.Raw 2 16
        | NumberKind.Octal -> parseSignedWithBase literal.Raw 2 8
        | NumberKind.Binary -> parseSignedWithBase literal.Raw 2 2
        | NumberKind.Special special ->
            match special with
            | SpecialNumberKind.Infinity -> Ok Double.PositiveInfinity
            | SpecialNumberKind.NegativeInfinity -> Ok Double.NegativeInfinity
            | SpecialNumberKind.NotANumber -> Ok Double.NaN

    let private keywordFromSpecial (special: SpecialNumberKind) =
        match special with
        | SpecialNumberKind.Infinity -> "#inf"
        | SpecialNumberKind.NegativeInfinity -> "#-inf"
        | SpecialNumberKind.NotANumber -> "#nan"

    let private specialFromKeyword keyword =
        match keyword with
        | "#inf" -> Ok SpecialNumberKind.Infinity
        | "#-inf" -> Ok SpecialNumberKind.NegativeInfinity
        | "#nan" -> Ok SpecialNumberKind.NotANumber
        | other -> Error $"Unsupported keyword literal: {other}"

    let private toIrValue (value: Value) : Result<IRValue, string> =
        match value with
        | Value.String(text, ty) ->
            Ok
                { TypeAnnotation = ty
                  Kind = ValueKind.String
                  Value = box text }
        | Value.Number(literal, ty) ->
            match literal.Kind with
            | NumberKind.Special special ->
                Ok
                    { TypeAnnotation = ty
                      Kind = ValueKind.Keyword
                      Value = box (keywordFromSpecial special) }
            | _ ->
                parseNumberLiteral literal
                |> Result.map (fun number ->
                    { TypeAnnotation = ty
                      Kind = ValueKind.Number
                      Value = box number })
        | Value.Boolean(b, ty) ->
            Ok
                { TypeAnnotation = ty
                  Kind = ValueKind.Bool
                  Value = box b }
        | Value.Null ty ->
            Ok
                { TypeAnnotation = ty
                  Kind = ValueKind.Null
                  Value = null }
        | Value.NodeValue _ -> Error "Node values are not supported for IR conversion yet."

    let private ofIrValue (value: IRValue) : Result<Value, string> =
        match value.Kind with
        | ValueKind.String -> Ok(Value.String(value.Value :?> string, value.TypeAnnotation))
        | ValueKind.Number ->
            let number = value.Value :?> float
            let raw = number.ToString("R", invariant)
            Ok(Value.Number(NumberLiteral.ofRaw raw, value.TypeAnnotation))
        | ValueKind.Bool -> Ok(Value.Boolean(value.Value :?> bool, value.TypeAnnotation))
        | ValueKind.Null -> Ok(Value.Null value.TypeAnnotation)
        | ValueKind.Keyword ->
            let keyword = value.Value :?> string

            specialFromKeyword keyword
            |> Result.map (fun special -> Value.Number(NumberLiteral.special keyword special, value.TypeAnnotation))

    let private combineResults items =
        items
        |> List.fold
            (fun acc next ->
                match acc, next with
                | Ok values, Ok value -> Ok(value :: values)
                | Error e, _ -> Error e
                | _, Error e -> Error e)
            (Ok [])
        |> Result.map List.rev

    let rec private toIrNode (node: Node) : Result<IRNode, string> =
        let argsResult = node.Arguments |> List.map toIrValue |> combineResults

        let propsResult =
            node.Properties
            |> List.map (fun prop -> toIrValue prop.Value |> Result.map (fun v -> prop.Key, v))
            |> combineResults
            |> Result.map Map.ofList

        let childrenResult = node.Children |> List.map toIrNode |> combineResults

        match argsResult, propsResult, childrenResult with
        | Ok args, Ok props, Ok children ->
            Ok
                { Name = node.Name
                  Type = node.TypeAnn
                  Arguments = args
                  Properties = props
                  Children = children }
        | Error e, _, _ -> Error e
        | _, Error e, _ -> Error e
        | _, _, Error e -> Error e

    let rec private ofIrNode (node: IRNode) : Result<Node, string> =
        let argsResult = node.Arguments |> List.map ofIrValue |> combineResults

        let propsResult =
            node.Properties
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (key, value) -> ofIrValue value |> Result.map (fun v -> { Key = key; Value = v }))
            |> combineResults

        let childrenResult = node.Children |> List.map ofIrNode |> combineResults

        match argsResult, propsResult, childrenResult with
        | Ok args, Ok props, Ok children ->
            Ok
                { TypeAnn = node.Type
                  Name = node.Name
                  Arguments = args
                  Properties = props
                  Children = children
                  Span = None
                  OriginalText = None }
        | Error e, _, _ -> Error e
        | _, Error e, _ -> Error e
        | _, _, Error e -> Error e

    /// Convert a parsed AST document into its IR representation.
    let toIR (document: Document) : Result<IRDocument, string> =
        document |> List.map toIrNode |> combineResults

    /// Convert an IR document back into the AST representation.
    let ofIR (ir: IRDocument) : Result<Document, string> =
        ir |> List.map ofIrNode |> combineResults
