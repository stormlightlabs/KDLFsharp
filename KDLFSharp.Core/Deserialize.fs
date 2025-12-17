namespace KDLFSharp.Core

open System
open Microsoft.FSharp.Reflection
open KDLFSharp.Core.Serialize

/// Record deserialization for mapping KDL documents back to F# records.
/// Supports primitive types, nested records, options, lists, and arrays.
module Deserialize =

    /// Get the simple name of a type without namespace/assembly info
    let rec private getTypeName (t: Type) =
        if t.IsGenericType then
            let baseName = t.Name.Substring(0, t.Name.IndexOf('`'))
            let args = t.GetGenericArguments() |> Array.map getTypeName |> String.concat ","
            $"{baseName}<{args}>"
        else
            t.Name

    /// Convert a KDL Value to a .NET object of the specified type
    let rec private kdlToValue (targetType: Type) (value: Value) : SerializeResult<obj> =
        match value with
        | Value.Null _ -> Ok null
        | Value.String(s, _) ->
            match targetType with
            | t when t = typeof<string> -> Ok(box s)
            | _ -> Error(TypeMismatch(targetType.Name, "string"))
        | Value.Boolean(b, _) ->
            match targetType with
            | t when t = typeof<bool> -> Ok(box b)
            | _ -> Error(TypeMismatch(targetType.Name, "bool"))
        | Value.Number(lit, _) ->
            let parseNumber () =
                match lit.Kind with
                | NumberKind.Special special ->
                    match special with
                    | SpecialNumberKind.Infinity -> Ok(box Double.PositiveInfinity)
                    | SpecialNumberKind.NegativeInfinity -> Ok(box Double.NegativeInfinity)
                    | SpecialNumberKind.NotANumber -> Ok(box Double.NaN)
                | _ ->
                    let raw = lit.Raw.Replace("_", "")

                    try
                        match targetType with
                        | t when t = typeof<int> -> Ok(box (Int32.Parse(raw)))
                        | t when t = typeof<int64> -> Ok(box (Int64.Parse(raw)))
                        | t when t = typeof<float> -> Ok(box (Double.Parse(raw)))
                        | t when t = typeof<float32> -> Ok(box (Single.Parse(raw)))
                        | t when t = typeof<decimal> -> Ok(box (Decimal.Parse(raw)))
                        | t when t = typeof<byte> -> Ok(box (Byte.Parse(raw)))
                        | t when t = typeof<int16> -> Ok(box (Int16.Parse(raw)))
                        | t when t = typeof<uint16> -> Ok(box (UInt16.Parse(raw)))
                        | t when t = typeof<uint32> -> Ok(box (UInt32.Parse(raw)))
                        | t when t = typeof<uint64> -> Ok(box (UInt64.Parse(raw)))
                        | _ -> Error(UnsupportedType(targetType.Name))
                    with ex ->
                        Error(InvalidValue $"Cannot parse '{raw}' as {targetType.Name}: {ex.Message}")

            parseNumber ()
        | Value.NodeValue _ -> Error(UnsupportedType "NodeValue not supported for deserialization")

    /// Convert a list of objects to a properly typed F# list
    let private createTypedList (elemType: Type) (items: obj list) : obj =
        let arr = Array.CreateInstance(elemType, items.Length)
        items |> List.iteri (fun idx item -> arr.SetValue(item, idx))

        typeof<obj list>.Assembly.GetTypes()
        |> Array.find (fun t -> t.Name = "ListModule" && t.Namespace = "Microsoft.FSharp.Collections")
        |> _.GetMethod("OfArray").MakeGenericMethod([| elemType |])
        |> _.Invoke(null, [| arr |])

    /// Deserialize a KDL Node to an F# record of the specified type (non-generic helper)
    let rec private fromNodeDynamic (config: SerializeConfig) (targetType: Type) (node: Node) : SerializeResult<obj> =
        if not (FSharpType.IsRecord targetType) then
            Error(UnsupportedType $"{getTypeName targetType} is not a record type")
        else
            let fields = FSharpType.GetRecordFields(targetType)
            let fieldValues = Array.zeroCreate<obj> fields.Length
            let mutable error = None
            let propMap = node.Properties |> List.map (fun p -> p.Key, p.Value) |> Map.ofList
            let childrenByName = node.Children |> List.groupBy (fun n -> n.Name) |> Map.ofList

            for i in 0 .. fields.Length - 1 do
                if error.IsNone then
                    let field = fields[i]
                    let fieldType = field.PropertyType

                    let isOption, innerType =
                        if
                            fieldType.IsGenericType
                            && fieldType.GetGenericTypeDefinition() = typedefof<option<_>>
                        then
                            true, fieldType.GetGenericArguments()[0]
                        else
                            false, fieldType

                    match Map.tryFind field.Name propMap with
                    | Some value ->
                        match kdlToValue innerType value with
                        | Ok v ->
                            if isOption then
                                if v = null then
                                    fieldValues[i] <- null
                                else
                                    let someType = typedefof<option<_>>.MakeGenericType([| innerType |])

                                    let unionCase =
                                        FSharpType.GetUnionCases(someType) |> Array.find (fun c -> c.Name = "Some")

                                    fieldValues[i] <- FSharpValue.MakeUnion(unionCase, [| v |])
                            else
                                fieldValues[i] <- v
                        | Error e -> error <- Some e
                    | None when FSharpType.IsRecord innerType ->
                        match Map.tryFind innerType.Name childrenByName with
                        | Some(childNode :: _) ->
                            match fromNodeDynamic config innerType childNode with
                            | Ok v ->
                                if isOption then
                                    let someType = typedefof<option<_>>.MakeGenericType([| innerType |])

                                    let unionCase =
                                        FSharpType.GetUnionCases(someType) |> Array.find (fun c -> c.Name = "Some")

                                    fieldValues[i] <- FSharpValue.MakeUnion(unionCase, [| v |])
                                else
                                    fieldValues[i] <- v
                            | Error e -> error <- Some e
                        | _ ->
                            if isOption then
                                fieldValues[i] <- null
                            else
                                error <- Some(MissingRequiredField field.Name)
                    | None when
                        innerType.IsArray
                        || innerType.IsGenericType
                           && innerType.GetGenericTypeDefinition() = typedefof<list<_>>
                        ->
                        let elemType =
                            if innerType.IsArray then
                                innerType.GetElementType()
                            else
                                innerType.GetGenericArguments()[0]

                        let fieldChildren = node.Children |> List.filter (fun n -> n.Name = field.Name)

                        if FSharpType.IsRecord elemType then
                            match fieldChildren with
                            | [ wrapperNode ] ->
                                let childResults =
                                    wrapperNode.Children
                                    |> List.map (fun childNode -> fromNodeDynamic config elemType childNode)

                                match childResults |> List.tryFind Result.isError with
                                | Some(Error e) -> error <- Some e
                                | _ ->
                                    let items =
                                        childResults
                                        |> List.choose (function
                                            | Ok v -> Some v
                                            | _ -> None)

                                    if innerType.IsArray then
                                        let arr = Array.CreateInstance(elemType, items.Length)
                                        items |> List.iteri (fun idx item -> arr.SetValue(item, idx))
                                        fieldValues[i] <- arr
                                    else
                                        fieldValues[i] <- createTypedList elemType items
                            | [] ->
                                if innerType.IsArray then
                                    fieldValues[i] <- Array.CreateInstance(elemType, 0)
                                else
                                    fieldValues[i] <- createTypedList elemType []
                            | _ ->
                                error <- Some(InvalidValue $"Expected single wrapper node for list field {field.Name}")
                        else
                            let valueResults =
                                fieldChildren
                                |> List.collect (fun childNode -> childNode.Arguments)
                                |> List.map (fun arg -> kdlToValue elemType arg)

                            match valueResults |> List.tryFind Result.isError with
                            | Some(Error e) -> error <- Some e
                            | _ ->
                                let items =
                                    valueResults
                                    |> List.choose (function
                                        | Ok v -> Some v
                                        | _ -> None)

                                if innerType.IsArray then
                                    let arr = Array.CreateInstance(elemType, items.Length)
                                    items |> List.iteri (fun idx item -> arr.SetValue(item, idx))
                                    fieldValues[i] <- arr
                                else
                                    fieldValues[i] <- createTypedList elemType items
                    | None ->
                        if isOption then
                            fieldValues[i] <- null
                        else
                            error <- MissingRequiredField field.Name |> Some

            match error with
            | Some e -> Error e
            | None ->
                try
                    FSharpValue.MakeRecord(targetType, fieldValues) |> Ok
                with ex ->
                    DeserializationFailed $"Failed to create record: {ex.Message}" |> Error

    /// Deserialize a KDL Node to an F# record of type 'T
    let fromNode<'T> (config: SerializeConfig) (node: Node) : SerializeResult<'T> =
        match fromNodeDynamic config typeof<'T> node with
        | Ok obj -> obj :?> 'T |> Ok
        | Error e -> Error e

    /// Deserialize a KDL document to an F# record (expects single-node document)
    let fromDocument<'T> (config: SerializeConfig) (document: Document) : SerializeResult<'T> =
        match document with
        | [] -> InvalidValue "Document is empty" |> Error
        | [ node ] -> fromNode<'T> config node
        | _ -> InvalidValue "Document contains multiple root nodes" |> Error

    /// Deserialize a KDL string to an F# record
    let fromString<'T> (config: SerializeConfig) (kdl: string) : SerializeResult<'T> =
        match Parser.parse kdl with
        | Ok document -> fromDocument<'T> config document
        | Error errors ->
            errors
            |> List.map (fun e -> e.Message)
            |> String.concat "; "
            |> DeserializationFailed
            |> Error
