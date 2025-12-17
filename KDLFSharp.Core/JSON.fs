namespace KDLFSharp.Core

open System
open System.Text.Json
open KDLFSharp.Core.IR

/// JSON serialization for the KDL Intermediate Representation.
/// Parses JSON documents into IR and emits IR as JSON using the canonical format.
module JSON =
    /// Parse a value kind string into ValueKind enum
    let private parseValueKind (kind: string) : Result<ValueKind, string> =
        match kind with
        | "string" -> Ok String
        | "number" -> Ok Number
        | "bool" -> Ok Bool
        | "null" -> Ok Null
        | "kw" -> Ok Keyword
        | _ -> Error $"Unknown value kind: {kind}"

    /// Parse an IRValue from a JSON element
    let private parseValue (elem: JsonElement) : Result<IRValue, string> =
        try
            if elem.ValueKind <> JsonValueKind.Object then
                Error "Value must be a JSON object"
            else
                let mutable kProp = Unchecked.defaultof<JsonElement>
                let mutable vProp = Unchecked.defaultof<JsonElement>
                let mutable tProp = Unchecked.defaultof<JsonElement>

                let hasK = elem.TryGetProperty("k", &kProp)
                let hasV = elem.TryGetProperty("v", &vProp)
                let hasT = elem.TryGetProperty("t", &tProp)

                if not hasK then
                    Error "Value object must have 'k' property"
                elif not hasV then
                    Error "Value object must have 'v' property"
                else
                    let kindStr = kProp.GetString()

                    match parseValueKind kindStr with
                    | Error e -> Error e
                    | Ok kind ->
                        let typeAnnotation =
                            if hasT && tProp.ValueKind <> JsonValueKind.Null then
                                Some(tProp.GetString())
                            else
                                None

                        let parseResult =
                            match kind with
                            | String -> Ok(box (vProp.GetString()))
                            | Number -> Ok(box (vProp.GetDouble()))
                            | Bool -> Ok(box (vProp.GetBoolean()))
                            | Null -> Ok null
                            | Keyword -> Ok(box (vProp.GetString()))

                        match parseResult with
                        | Ok v ->
                            Ok
                                { TypeAnnotation = typeAnnotation
                                  Kind = kind
                                  Value = v }
                        | Error e -> Error e
        with ex ->
            Error $"Error parsing value: {ex.Message}"

    /// Parse an IRNode from a JSON element
    let rec private parseNode (elem: JsonElement) : Result<IRNode, string> =
        try
            if elem.ValueKind <> JsonValueKind.Object then
                Error "Node must be a JSON object"
            else
                let mutable nameProp = Unchecked.defaultof<JsonElement>
                let mutable typeProp = Unchecked.defaultof<JsonElement>
                let mutable argsProp = Unchecked.defaultof<JsonElement>
                let mutable propsProp = Unchecked.defaultof<JsonElement>
                let mutable childrenProp = Unchecked.defaultof<JsonElement>

                let hasName = elem.TryGetProperty("name", &nameProp)
                let hasType = elem.TryGetProperty("type", &typeProp)
                let hasArgs = elem.TryGetProperty("args", &argsProp)
                let hasProps = elem.TryGetProperty("props", &propsProp)
                let hasChildren = elem.TryGetProperty("children", &childrenProp)

                if not hasName then
                    Error "Node must have 'name' property"
                else
                    let name = nameProp.GetString()

                    let nodeType =
                        if hasType && typeProp.ValueKind <> JsonValueKind.Null then
                            Some(typeProp.GetString())
                        else
                            None

                    let argsResult =
                        if not hasArgs || argsProp.ValueKind <> JsonValueKind.Array then
                            Ok []
                        else
                            argsProp.EnumerateArray()
                            |> Seq.map parseValue
                            |> Seq.toList
                            |> List.fold
                                (fun acc r ->
                                    match acc, r with
                                    | Ok lst, Ok v -> Ok(v :: lst)
                                    | Error e, _ -> Error e
                                    | _, Error e -> Error e)
                                (Ok [])
                            |> Result.map List.rev

                    let propsResult =
                        if not hasProps || propsProp.ValueKind <> JsonValueKind.Object then
                            Ok Map.empty
                        else
                            propsProp.EnumerateObject()
                            |> Seq.map (fun prop ->
                                match parseValue prop.Value with
                                | Ok v -> Ok(prop.Name, v)
                                | Error e -> Error e)
                            |> Seq.toList
                            |> List.fold
                                (fun acc r ->
                                    match acc, r with
                                    | Ok map, Ok(k, v) -> Ok(Map.add k v map)
                                    | Error e, _ -> Error e
                                    | _, Error e -> Error e)
                                (Ok Map.empty)

                    let childrenResult =
                        if not hasChildren || childrenProp.ValueKind <> JsonValueKind.Array then
                            Ok []
                        else
                            childrenProp.EnumerateArray()
                            |> Seq.map parseNode
                            |> Seq.toList
                            |> List.fold
                                (fun acc r ->
                                    match acc, r with
                                    | Ok lst, Ok n -> Ok(n :: lst)
                                    | Error e, _ -> Error e
                                    | _, Error e -> Error e)
                                (Ok [])
                            |> Result.map List.rev

                    match argsResult, propsResult, childrenResult with
                    | Ok args, Ok props, Ok children ->
                        Ok
                            { Name = name
                              Type = nodeType
                              Arguments = args
                              Properties = props
                              Children = children }
                    | Error e, _, _ -> Error e
                    | _, Error e, _ -> Error e
                    | _, _, Error e -> Error e
        with ex ->
            Error $"Error parsing node: {ex.Message}"

    /// Parse JSON string into IR document
    let parseJSON (json: string) : Result<IRDocument, string> =
        try
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement

            if root.ValueKind <> JsonValueKind.Array then
                Error "document must be an array"
            else
                root.EnumerateArray()
                |> Seq.map parseNode
                |> Seq.toList
                |> List.fold
                    (fun acc r ->
                        match acc, r with
                        | Ok lst, Ok n -> Ok(n :: lst)
                        | Error e, _ -> Error e
                        | _, Error e -> Error e)
                    (Ok [])
                |> Result.map List.rev
        with ex ->
            Error $"JSON parse error: {ex.Message}"

    /// Emit a value kind as string
    let private emitValueKind (kind: ValueKind) : string =
        match kind with
        | String -> "string"
        | Number -> "number"
        | Bool -> "bool"
        | Null -> "null"
        | Keyword -> "kw"

    /// Emit an IRValue as a JSON object
    let private emitValue (writer: Utf8JsonWriter) (value: IRValue) : unit =
        writer.WriteStartObject()

        match value.TypeAnnotation with
        | Some t -> writer.WriteString("t", t)
        | None -> writer.WriteNull("t")

        writer.WriteString("k", emitValueKind value.Kind)

        match value.Kind with
        | String -> writer.WriteString("v", value.Value :?> string)
        | Number -> writer.WriteNumber("v", value.Value :?> float)
        | Bool -> writer.WriteBoolean("v", value.Value :?> bool)
        | Null -> writer.WriteNull("v")
        | Keyword -> writer.WriteString("v", value.Value :?> string)

        writer.WriteEndObject()

    /// Emit an IRNode as a JSON object
    let rec private emitNode (writer: Utf8JsonWriter) (node: IRNode) : unit =
        writer.WriteStartObject()
        writer.WriteString("name", node.Name)

        match node.Type with
        | Some t -> writer.WriteString("type", t)
        | None -> writer.WriteNull("type")

        writer.WriteStartArray("args")
        node.Arguments |> List.iter (emitValue writer)
        writer.WriteEndArray()

        writer.WriteStartObject("props")

        node.Properties
        |> Map.iter (fun key value ->
            writer.WritePropertyName(key)
            emitValue writer value)

        writer.WriteEndObject()

        writer.WriteStartArray("children")
        node.Children |> List.iter (emitNode writer)
        writer.WriteEndArray()

        writer.WriteEndObject()

    /// Emit IR document as JSON string
    let emitJSON (ir: IRDocument) : string =
        use stream = new System.IO.MemoryStream()
        use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

        writer.WriteStartArray()
        ir |> List.iter (emitNode writer)
        writer.WriteEndArray()

        writer.Flush()
        System.Text.Encoding.UTF8.GetString(stream.ToArray())
