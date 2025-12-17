namespace KDLFSharp.Core

open System
open System.Text.Json

/// Structured JSON serialization for full KDL documents (non-IR).
module StructuredJSON =
    let private writeTypeAnnotation (writer: Utf8JsonWriter) (tyOpt: string option) =
        match tyOpt with
        | Some ty -> writer.WriteString("typeAnnotation", ty)
        | None -> writer.WriteNull("typeAnnotation")

    let private writeValue (writer: Utf8JsonWriter) (value: Value) =
        writer.WriteStartObject()

        match value with
        | Value.String(text, ty) ->
            writer.WriteString("kind", "string")
            writeTypeAnnotation writer ty
            writer.WriteString("value", text)
        | Value.Number(literal, ty) ->
            writer.WriteString("kind", "number")
            writeTypeAnnotation writer ty
            writer.WriteString("literal", literal.Raw)
        | Value.Boolean(b, ty) ->
            writer.WriteString("kind", "bool")
            writeTypeAnnotation writer ty
            writer.WriteBoolean("value", b)
        | Value.Null ty ->
            writer.WriteString("kind", "null")
            writeTypeAnnotation writer ty
        | Value.NodeValue _ -> raise (NotSupportedException("Node values are not supported in structured JSON."))

        writer.WriteEndObject()

    let rec private writeNode (writer: Utf8JsonWriter) (node: Node) =
        writer.WriteStartObject()
        writer.WriteString("name", node.Name)

        match node.TypeAnn with
        | Some ty -> writer.WriteString("typeAnnotation", ty)
        | None -> writer.WriteNull("typeAnnotation")

        writer.WriteStartArray("arguments")
        node.Arguments |> List.iter (writeValue writer)
        writer.WriteEndArray()

        writer.WriteStartArray("properties")

        node.Properties
        |> List.iter (fun prop ->
            writer.WriteStartObject()
            writer.WriteString("key", prop.Key)
            writer.WritePropertyName("value")
            writeValue writer prop.Value
            writer.WriteEndObject())

        writer.WriteEndArray()

        writer.WriteStartArray("children")
        node.Children |> List.iter (writeNode writer)
        writer.WriteEndArray()

        writer.WriteEndObject()

    /// Emit a document as structured JSON string.
    let emitDocument (document: Document) =
        use stream = new System.IO.MemoryStream()
        use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

        writer.WriteStartObject()
        writer.WriteStartArray("nodes")
        document |> List.iter (writeNode writer)
        writer.WriteEndArray()
        writer.WriteEndObject()

        writer.Flush()
        Text.Encoding.UTF8.GetString(stream.ToArray())

    let private readType (elem: JsonElement) (name: string) =
        let mutable prop = Unchecked.defaultof<JsonElement>

        if elem.TryGetProperty(name, &prop) then
            if prop.ValueKind = JsonValueKind.Null then
                None
            else
                Some(prop.GetString())
        else
            None

    let private readValue (elem: JsonElement) : Result<Value, string> =
        let mutable kindProp = Unchecked.defaultof<JsonElement>

        if not (elem.TryGetProperty("kind", &kindProp)) then
            Error "value is missing 'kind'"
        else
            let kind = kindProp.GetString()
            let ty = readType elem "typeAnnotation"

            match kind with
            | "string" ->
                let mutable valueProp = Unchecked.defaultof<JsonElement>

                if elem.TryGetProperty("value", &valueProp) then
                    Ok(Value.String(valueProp.GetString(), ty))
                else
                    Error "string value must include 'value'"
            | "number" ->
                let mutable literalProp = Unchecked.defaultof<JsonElement>

                if elem.TryGetProperty("literal", &literalProp) then
                    let literal = literalProp.GetString()

                    let numberLiteral =
                        match literal with
                        | "#inf" -> NumberLiteral.special literal SpecialNumberKind.Infinity
                        | "#-inf" -> NumberLiteral.special literal SpecialNumberKind.NegativeInfinity
                        | "#nan" -> NumberLiteral.special literal SpecialNumberKind.NotANumber
                        | _ -> NumberLiteral.ofRaw literal

                    Ok(Value.Number(numberLiteral, ty))
                else
                    Error "number value must include 'literal'"
            | "bool" ->
                let mutable boolProp = Unchecked.defaultof<JsonElement>

                if elem.TryGetProperty("value", &boolProp) then
                    Ok(Value.Boolean(boolProp.GetBoolean(), ty))
                else
                    Error "bool value must include 'value'"
            | "null" -> Ok(Value.Null ty)
            | other -> Error $"unsupported value kind: {other}"

    let rec private readNode (elem: JsonElement) : Result<Node, string> =
        let mutable nameProp = Unchecked.defaultof<JsonElement>

        if not (elem.TryGetProperty("name", &nameProp)) then
            Error "node is missing 'name'"
        else
            let name = nameProp.GetString()
            let ty = readType elem "typeAnnotation"

            let argsResult =
                let mutable argsProp = Unchecked.defaultof<JsonElement>

                if
                    elem.TryGetProperty("arguments", &argsProp)
                    && argsProp.ValueKind = JsonValueKind.Array
                then
                    argsProp.EnumerateArray()
                    |> Seq.map readValue
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok lst, Ok v -> Ok(v :: lst)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok [])
                    |> Result.map List.rev
                else
                    Ok []

            let propsResult =
                let mutable propsProp = Unchecked.defaultof<JsonElement>

                if
                    elem.TryGetProperty("properties", &propsProp)
                    && propsProp.ValueKind = JsonValueKind.Array
                then
                    propsProp.EnumerateArray()
                    |> Seq.map (fun propElem ->
                        let mutable keyProp = Unchecked.defaultof<JsonElement>
                        let mutable valueProp = Unchecked.defaultof<JsonElement>

                        if not (propElem.TryGetProperty("key", &keyProp)) then
                            Error "property missing 'key'"
                        elif not (propElem.TryGetProperty("value", &valueProp)) then
                            Error "property missing 'value'"
                        else
                            match readValue valueProp with
                            | Ok v -> Ok({ Key = keyProp.GetString(); Value = v })
                            | Error e -> Error e)
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok lst, Ok v -> Ok(v :: lst)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok [])
                    |> Result.map List.rev
                else
                    Ok []

            let childrenResult =
                let mutable childrenProp = Unchecked.defaultof<JsonElement>

                if
                    elem.TryGetProperty("children", &childrenProp)
                    && childrenProp.ValueKind = JsonValueKind.Array
                then
                    childrenProp.EnumerateArray()
                    |> Seq.map readNode
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok lst, Ok v -> Ok(v :: lst)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok [])
                    |> Result.map List.rev
                else
                    Ok []

            match argsResult, propsResult, childrenResult with
            | Ok args, Ok props, Ok children ->
                Ok
                    { TypeAnn = ty
                      Name = name
                      Arguments = args
                      Properties = props
                      Children = children
                      Span = None
                      OriginalText = None }
            | Error e, _, _ -> Error e
            | _, Error e, _ -> Error e
            | _, _, Error e -> Error e

    /// Parse structured JSON string into a KDL document.
    let parseDocument (json: string) : Result<Document, string> =
        try
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement

            if root.ValueKind <> JsonValueKind.Object then
                Error "document must be a JSON object"
            else
                let mutable nodesProp = Unchecked.defaultof<JsonElement>

                if
                    not (root.TryGetProperty("nodes", &nodesProp))
                    || nodesProp.ValueKind <> JsonValueKind.Array
                then
                    Error "document must have a 'nodes' array"
                else
                    nodesProp.EnumerateArray()
                    |> Seq.map readNode
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok lst, Ok node -> Ok(node :: lst)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok [])
                    |> Result.map List.rev
        with ex ->
            Error $"JSON parse error: {ex.Message}"
