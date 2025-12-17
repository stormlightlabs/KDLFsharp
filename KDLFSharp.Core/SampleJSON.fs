namespace KDLFSharp.Core

open System
open System.Globalization
open System.Text.Json

/// Emits and parses a friendlier, sample-style JSON representation (lossy).
module SampleJSON =
    let private invariant = CultureInfo.InvariantCulture

    let private tryWriteNumberLiteral (writer: Utf8JsonWriter) (raw: string) =
        match Double.TryParse(raw, NumberStyles.Float, invariant) with
        | true, value -> writer.WriteNumberValue(value)
        | _ -> writer.WriteStringValue(raw)

    let rec private writeValue (writer: Utf8JsonWriter) (value: Value) =
        match value with
        | Value.String(s, _) -> writer.WriteStringValue(s)
        | Value.Number(lit, _) ->
            if lit.Raw.StartsWith("#") then
                writer.WriteStringValue(lit.Raw)
            else
                tryWriteNumberLiteral writer lit.Raw
        | Value.Boolean(b, _) -> writer.WriteBooleanValue(b)
        | Value.Null _ -> writer.WriteNullValue()
        | Value.NodeValue _ -> writer.WriteStringValue("<node>")

    let private writeArguments (writer: Utf8JsonWriter) (args: Value list) =
        if not args.IsEmpty then
            writer.WriteStartArray("_args")
            args |> List.iter (writeValue writer)
            writer.WriteEndArray()

    let private writeMetadata (writer: Utf8JsonWriter) (node: Node) =
        let propertyTypes =
            node.Properties
            |> List.choose (fun prop ->
                match prop.Value with
                | Value.String(_, Some ty)
                | Value.Number(_, Some ty)
                | Value.Boolean(_, Some ty)
                | Value.Null(Some ty) -> Some(prop.Key, ty)
                | _ -> None)

        let argumentTypes =
            node.Arguments
            |> List.mapi (fun idx arg ->
                let ty =
                    match arg with
                    | Value.String(_, ty)
                    | Value.Number(_, ty)
                    | Value.Boolean(_, ty)
                    | Value.Null ty -> ty
                    | Value.NodeValue(_, ty) -> ty

                idx, ty)
            |> List.filter (fun (_, ty) -> ty.IsSome)

        if
            node.TypeAnn.IsSome
            || (not propertyTypes.IsEmpty)
            || (not argumentTypes.IsEmpty)
        then
            writer.WriteStartObject("_meta")

            node.TypeAnn |> Option.iter (fun ty -> writer.WriteString("typeAnnotation", ty))

            if not argumentTypes.IsEmpty then
                writer.WriteStartArray("argumentTypes")

                argumentTypes
                |> List.iter (fun (idx, ty) ->
                    writer.WriteStartObject()
                    writer.WriteNumber("index", idx)
                    ty |> Option.iter (fun t -> writer.WriteString("type", t))
                    writer.WriteEndObject())

                writer.WriteEndArray()

            if not propertyTypes.IsEmpty then
                writer.WriteStartObject("propertyTypes")
                propertyTypes |> List.iter (fun (key, ty) -> writer.WriteString(key, ty))
                writer.WriteEndObject()

            writer.WriteEndObject()

    let rec private writeNode (writer: Utf8JsonWriter) (node: Node) =
        writer.WriteStartObject()
        writeArguments writer node.Arguments
        writeMetadata writer node

        node.Properties
        |> List.iter (fun prop ->
            writer.WritePropertyName(prop.Key)
            writeValue writer prop.Value)

        node.Children
        |> List.groupBy (fun child -> child.Name)
        |> List.iter (fun (name, children) ->
            writer.WritePropertyName(name)

            match children with
            | [ single ] -> writeNode writer single
            | many ->
                writer.WriteStartArray()
                many |> List.iter (writeNode writer)
                writer.WriteEndArray())

        writer.WriteEndObject()

    let emitDocument (document: Document) =
        use stream = new System.IO.MemoryStream()
        use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

        match document |> List.groupBy (fun node -> node.Name) with
        | [] ->
            writer.WriteStartObject()
            writer.WriteEndObject()
        | groups ->
            writer.WriteStartObject()

            groups
            |> List.iter (fun (name, nodes) ->
                writer.WritePropertyName(name)

                match nodes with
                | [ single ] -> writeNode writer single
                | many ->
                    writer.WriteStartArray()
                    many |> List.iter (writeNode writer)
                    writer.WriteEndArray())

            writer.WriteEndObject()

        writer.Flush()
        System.Text.Encoding.UTF8.GetString(stream.ToArray())

    type private NodeMeta =
        { TypeAnnotation: string option
          ArgumentTypes: Map<int, string>
          PropertyTypes: Map<string, string> }

    let private emptyMeta =
        { TypeAnnotation = None
          ArgumentTypes = Map.empty
          PropertyTypes = Map.empty }

    let private tryGetProperty (elem: JsonElement) (name: string) =
        let mutable prop = Unchecked.defaultof<JsonElement>
        if elem.TryGetProperty(name, &prop) then Some prop else None

    let private readMeta (elem: JsonElement) =
        match tryGetProperty elem "_meta" with
        | None -> emptyMeta
        | Some metaElem ->
            let typeAnnotation =
                match tryGetProperty metaElem "typeAnnotation" with
                | Some ta when ta.ValueKind = JsonValueKind.String -> Some(ta.GetString())
                | _ -> None

            let argumentTypes =
                match tryGetProperty metaElem "argumentTypes" with
                | Some array when array.ValueKind = JsonValueKind.Array ->
                    array.EnumerateArray()
                    |> Seq.choose (fun entry ->
                        let mutable idxProp = Unchecked.defaultof<JsonElement>

                        if
                            entry.TryGetProperty("index", &idxProp)
                            && idxProp.ValueKind = JsonValueKind.Number
                        then
                            let idx = idxProp.GetInt32()
                            let mutable typeProp = Unchecked.defaultof<JsonElement>

                            if
                                entry.TryGetProperty("type", &typeProp)
                                && typeProp.ValueKind = JsonValueKind.String
                            then
                                Some(idx, typeProp.GetString())
                            else
                                None
                        else
                            None)
                    |> Map.ofSeq
                | _ -> Map.empty

            let propertyTypes =
                match tryGetProperty metaElem "propertyTypes" with
                | Some obj when obj.ValueKind = JsonValueKind.Object ->
                    obj.EnumerateObject()
                    |> Seq.choose (fun prop ->
                        if prop.Value.ValueKind = JsonValueKind.String then
                            Some(prop.Name, prop.Value.GetString())
                        else
                            None)
                    |> Map.ofSeq
                | _ -> Map.empty

            { TypeAnnotation = typeAnnotation
              ArgumentTypes = argumentTypes
              PropertyTypes = propertyTypes }

    let private parseScalarValue (elem: JsonElement) tyOpt =
        let value =
            match elem.ValueKind with
            | JsonValueKind.String ->
                match elem.GetString() with
                | "#true" -> Value.Boolean(true, None)
                | "#false" -> Value.Boolean(false, None)
                | "#null" -> Value.Null None
                | str -> Value.String(str, None)
            | JsonValueKind.Number -> Value.Number(NumberLiteral.ofRaw (elem.GetRawText()), None)
            | JsonValueKind.True -> Value.Boolean(true, None)
            | JsonValueKind.False -> Value.Boolean(false, None)
            | JsonValueKind.Null -> Value.Null None
            | _ -> Value.String(elem.GetRawText(), None)

        match value with
        | Value.String(str, _) -> Value.String(str, tyOpt)
        | Value.Number(lit, _) -> Value.Number(lit, tyOpt)
        | Value.Boolean(b, _) -> Value.Boolean(b, tyOpt)
        | Value.Null _ -> Value.Null tyOpt
        | Value.NodeValue(nodes, _) -> Value.NodeValue(nodes, tyOpt)

    let private parseArguments (elem: JsonElement) (meta: NodeMeta) =
        match tryGetProperty elem "_args" with
        | None -> Ok []
        | Some argsElem when argsElem.ValueKind = JsonValueKind.Array ->
            argsElem.EnumerateArray()
            |> Seq.mapi (fun idx argElem ->
                let ty = Map.tryFind idx meta.ArgumentTypes
                parseScalarValue argElem ty)
            |> Seq.toList
            |> Ok
        | _ -> Error "_args must be an array when present"

    let rec private parseNode name (elem: JsonElement) : Result<Node, string> =
        if elem.ValueKind <> JsonValueKind.Object then
            Error $"node '{name}' must be an object"
        else
            let meta = readMeta elem

            match parseArguments elem meta with
            | Error e -> Error e
            | Ok args ->
                let props = ResizeArray<Property>()
                let children = ResizeArray<Node>()
                let mutable error: string option = None

                let addProperty propName (valueElem: JsonElement) =
                    let ty = Map.tryFind propName meta.PropertyTypes
                    let value = parseScalarValue valueElem ty
                    props.Add({ Key = propName; Value = value })

                let rec addChildNodes childName (valueElem: JsonElement) =
                    match parseNode childName valueElem with
                    | Ok node -> children.Add(node)
                    | Error e -> error <- Some e

                elem.EnumerateObject()
                |> Seq.iter (fun prop ->
                    if error.IsNone then
                        match prop.Name with
                        | "_args"
                        | "_meta" -> ()
                        | _ ->
                            match prop.Value.ValueKind with
                            | JsonValueKind.Object -> addChildNodes prop.Name prop.Value
                            | JsonValueKind.Array ->
                                let elements = prop.Value.EnumerateArray() |> Seq.toList

                                if elements |> List.forall (fun e -> e.ValueKind = JsonValueKind.Object) then
                                    elements |> List.iter (addChildNodes prop.Name)
                                else
                                    addProperty prop.Name prop.Value
                            | _ -> addProperty prop.Name prop.Value)

                match error with
                | Some e -> Error e
                | None ->
                    Ok
                        { TypeAnn = meta.TypeAnnotation
                          Name = name
                          Arguments = args
                          Properties = List.ofSeq props
                          Children = List.ofSeq children
                          Span = None
                          OriginalText = None }

    let parseDocument (json: string) : Result<Document, string> =
        try
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement

            if root.ValueKind <> JsonValueKind.Object then
                Error "Sample JSON document must be an object"
            else
                root.EnumerateObject()
                |> Seq.map (fun prop ->
                    if prop.Value.ValueKind = JsonValueKind.Array then
                        let elements = prop.Value.EnumerateArray() |> Seq.toList

                        if elements |> List.forall (fun e -> e.ValueKind = JsonValueKind.Object) then
                            elements |> List.map (parseNode prop.Name)
                        else
                            [ Error $"array '{prop.Name}' must contain objects" ]
                    elif prop.Value.ValueKind = JsonValueKind.Object then
                        [ parseNode prop.Name prop.Value ]
                    else
                        [ Error $"root property '{prop.Name}' must be an object or array of objects" ])
                |> Seq.concat
                |> Seq.fold
                    (fun state result ->
                        match state, result with
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                        | Ok acc, Ok node -> Ok(node :: acc))
                    (Ok [])
                |> Result.map List.rev
        with ex ->
            Error $"JSON parse error: {ex.Message}"
