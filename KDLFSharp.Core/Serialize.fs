namespace KDLFSharp.Core

open System
open Microsoft.FSharp.Reflection

/// Record serialization for mapping F# records to KDL documents and back.
/// Supports primitive types, nested records, options, lists, and arrays.
module Serialize =
    /// Configuration for serialization behavior
    type SerializeConfig =
        {
            /// Use child nodes for nested records instead of inline properties
            NestedRecordsAsChildren: bool
            /// Use child nodes for list items instead of multiple arguments
            ListItemsAsChildren: bool
            /// Include type annotations for round-trip accuracy
            IncludeTypeAnnotations: bool
            /// Custom node name for the root record (defaults to record type name)
            RootNodeName: string option
        }

        static member Default =
            { NestedRecordsAsChildren = true
              ListItemsAsChildren = false
              IncludeTypeAnnotations = true
              RootNodeName = None }

    /// Errors that can occur during serialization/deserialization
    type SerializeError =
        | UnsupportedType of string
        | MissingRequiredField of string
        | TypeMismatch of expected: string * actual: string
        | InvalidValue of string
        | DeserializationFailed of string

        override this.ToString() =
            match this with
            | UnsupportedType t -> $"Unsupported type: {t}"
            | MissingRequiredField f -> $"Missing required field: {f}"
            | TypeMismatch(exp, act) -> $"Type mismatch: expected {exp}, got {act}"
            | InvalidValue v -> $"Invalid value: {v}"
            | DeserializationFailed msg -> $"Deserialization failed: {msg}"

    type SerializeResult<'T> = Result<'T, SerializeError>

    /// Get the simple name of a type without namespace/assembly info
    let rec private getTypeName (t: Type) =
        if t.IsGenericType then
            let baseName = t.Name.Substring(0, t.Name.IndexOf('`'))
            let args = t.GetGenericArguments() |> Array.map getTypeName |> String.concat ","
            $"{baseName}<{args}>"
        else
            t.Name

    /// Convert a primitive value to a KDL Value
    let private valueToKdl (typeAnn: string option) (value: obj) : SerializeResult<Value> =
        if value = null then
            Ok(Value.Null typeAnn)
        else
            let t = value.GetType()

            match value with
            | :? string as s -> Ok(Value.String(s, typeAnn))
            | :? bool as b -> Ok(Value.Boolean(b, typeAnn))
            | :? int as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | :? int64 as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | :? float as f when Double.IsPositiveInfinity f ->
                Ok(Value.Number(NumberLiteral.special "#inf" SpecialNumberKind.Infinity, typeAnn))
            | :? float as f when Double.IsNegativeInfinity f ->
                Ok(Value.Number(NumberLiteral.special "#-inf" SpecialNumberKind.NegativeInfinity, typeAnn))
            | :? float as f when Double.IsNaN f ->
                Ok(Value.Number(NumberLiteral.special "#nan" SpecialNumberKind.NotANumber, typeAnn))
            | :? float as f -> Ok(Value.Number(NumberLiteral.ofRaw (f.ToString("R")), typeAnn))
            | :? float32 as f -> Ok(Value.Number(NumberLiteral.ofRaw (f.ToString("R")), typeAnn))
            | :? decimal as d -> Ok(Value.Number(NumberLiteral.ofRaw (d.ToString()), typeAnn))
            | :? byte as b -> Ok(Value.Number(NumberLiteral.ofRaw (b.ToString()), typeAnn))
            | :? int16 as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | :? uint16 as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | :? uint32 as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | :? uint64 as i -> Ok(Value.Number(NumberLiteral.ofRaw (i.ToString()), typeAnn))
            | _ -> Error(UnsupportedType(getTypeName t))

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
                        || (innerType.IsGenericType
                            && innerType.GetGenericTypeDefinition() = typedefof<list<_>>)
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
                            error <- Some(MissingRequiredField field.Name)

            match error with
            | Some e -> Error e
            | None ->
                try
                    let record = FSharpValue.MakeRecord(targetType, fieldValues)
                    Ok record
                with ex ->
                    Error(DeserializationFailed $"Failed to create record: {ex.Message}")

    /// Convert a KDL Value to a .NET object of the specified type
    and private kdlToValue (targetType: Type) (value: Value) : SerializeResult<obj> =
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

    /// Serialize an F# record to a KDL Node
    let rec toNode (config: SerializeConfig) (value: obj) : SerializeResult<Node> =
        if value = null then
            Error(InvalidValue "Cannot serialize null as a node")
        else
            let t = value.GetType()

            if not (FSharpType.IsRecord t) then
                Error(UnsupportedType $"{getTypeName t} is not a record type")
            else
                let fields = FSharpType.GetRecordFields(t)
                let values = FSharpValue.GetRecordFields(value)

                let mutable arguments = []
                let mutable properties = []
                let mutable children = []
                let mutable error = None

                for i in 0 .. fields.Length - 1 do
                    if error.IsNone then
                        let field = fields[i]
                        let fieldValue = values[i]
                        let fieldType = field.PropertyType

                        let actualValue, actualType =
                            if
                                fieldType.IsGenericType
                                && fieldType.GetGenericTypeDefinition() = typedefof<option<_>>
                            then
                                if fieldValue = null then
                                    None, fieldType.GetGenericArguments()[0]
                                else
                                    let optionValue = fieldType.GetProperty("Value").GetValue(fieldValue)

                                    Some optionValue, fieldType.GetGenericArguments()[0]
                            else
                                Some fieldValue, fieldType

                        match actualValue with
                        | None -> ()
                        | Some v when v = null ->
                            properties <-
                                { Key = field.Name
                                  Value = Value.Null None }
                                :: properties
                        | Some v ->
                            let typeAnn =
                                if config.IncludeTypeAnnotations then
                                    Some(getTypeName actualType)
                                else
                                    None

                            if
                                actualType.IsArray
                                || (actualType.IsGenericType
                                    && actualType.GetGenericTypeDefinition() = typedefof<list<_>>)
                            then
                                let items =
                                    if actualType.IsArray then
                                        v :?> Array |> Seq.cast<obj> |> Seq.toList
                                    else
                                        v :?> System.Collections.IEnumerable |> Seq.cast<obj> |> Seq.toList

                                let elemType =
                                    if actualType.IsArray then
                                        actualType.GetElementType()
                                    else
                                        actualType.GetGenericArguments()[0]

                                if config.ListItemsAsChildren && FSharpType.IsRecord elemType then
                                    let childResults = items |> List.map (fun item -> toNode config item)

                                    match childResults |> List.tryFind (fun r -> Result.isError r) with
                                    | Some(Error e) -> error <- Some e
                                    | _ ->
                                        let childNodes =
                                            childResults
                                            |> List.choose (function
                                                | Ok n -> Some n
                                                | _ -> None)

                                        children <- children @ childNodes
                                elif FSharpType.IsRecord elemType then
                                    let childResults = items |> List.map (fun item -> toNode config item)

                                    match childResults |> List.tryFind (fun r -> Result.isError r) with
                                    | Some(Error e) -> error <- Some e
                                    | _ ->
                                        let recordNodes =
                                            childResults
                                            |> List.choose (function
                                                | Ok n -> Some n
                                                | _ -> None)

                                        let wrapperNode =
                                            { TypeAnn = None
                                              Name = field.Name
                                              Arguments = []
                                              Properties = []
                                              Children = recordNodes
                                              Span = None
                                              OriginalText = None }

                                        children <- wrapperNode :: children
                                else
                                    let childResults =
                                        items
                                        |> List.map (fun item ->
                                            match valueToKdl None item with
                                            | Ok kdlValue ->
                                                Ok
                                                    { TypeAnn = None
                                                      Name = field.Name
                                                      Arguments = [ kdlValue ]
                                                      Properties = []
                                                      Children = []
                                                      Span = None
                                                      OriginalText = None }
                                            | Error e -> Error e)

                                    match childResults |> List.tryFind (fun r -> Result.isError r) with
                                    | Some(Error e) -> error <- Some e
                                    | _ ->
                                        let childNodes =
                                            childResults
                                            |> List.choose (function
                                                | Ok n -> Some n
                                                | _ -> None)

                                        children <- children @ childNodes
                            elif FSharpType.IsRecord actualType then
                                match toNode config v with
                                | Ok childNode ->
                                    if config.NestedRecordsAsChildren then
                                        children <- childNode :: children
                                    else
                                        error <- Some(UnsupportedType "Flattening nested records not yet supported")
                                | Error e -> error <- Some e
                            else
                                match valueToKdl typeAnn v with
                                | Ok kdlValue -> properties <- { Key = field.Name; Value = kdlValue } :: properties
                                | Error e -> error <- Some e

                match error with
                | Some e -> Error e
                | None ->
                    let nodeName =
                        match config.RootNodeName with
                        | Some name -> name
                        | None -> t.Name

                    Ok
                        { TypeAnn =
                            if config.IncludeTypeAnnotations then
                                Some(getTypeName t)
                            else
                                None
                          Name = nodeName
                          Arguments = List.rev arguments
                          Properties = List.rev properties
                          Children = List.rev children
                          Span = None
                          OriginalText = None }

    /// Deserialize a KDL Node to an F# record of type 'T
    let fromNode<'T> (config: SerializeConfig) (node: Node) : SerializeResult<'T> =
        match fromNodeDynamic config typeof<'T> node with
        | Ok obj -> Ok(obj :?> 'T)
        | Error e -> Error e

    /// Serialize an F# record to a KDL document (single-node document)
    let toDocument (config: SerializeConfig) (value: obj) : SerializeResult<Document> =
        toNode config value |> Result.map (fun node -> [ node ])

    /// Deserialize a KDL document to an F# record (expects single-node document)
    let fromDocument<'T> (config: SerializeConfig) (document: Document) : SerializeResult<'T> =
        match document with
        | [] -> Error(InvalidValue "Document is empty")
        | [ node ] -> fromNode<'T> config node
        | _ -> Error(InvalidValue "Document contains multiple root nodes")

    /// Serialize an F# record to a KDL string
    let toString (config: SerializeConfig) (value: obj) : SerializeResult<string> =
        toDocument config value |> Result.map Render.renderDocument

    /// Deserialize a KDL string to an F# record
    let fromString<'T> (config: SerializeConfig) (kdl: string) : SerializeResult<'T> =
        match Parser.parse kdl with
        | Ok document -> fromDocument<'T> config document
        | Error errors ->
            let errorMsg = errors |> List.map (fun e -> e.Message) |> String.concat "; "

            Error(DeserializationFailed errorMsg)
