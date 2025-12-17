namespace KDLFSharp.Core

open System
open System.Xml.Linq
open KDLFSharp.Core.IR

/// XML serialization for the KDL Intermediate Representation.
/// Parses XML documents into IR and emits IR as XML using the canonical format.
module XML =

    let private xname (name: string) = XName.Get(name)

    /// Parse a value kind string into ValueKind enum
    let private parseValueKind (kind: string) : Result<ValueKind, string> =
        match kind with
        | "string" -> Ok String
        | "number" -> Ok Number
        | "bool" -> Ok Bool
        | "null" -> Ok Null
        | "kw" -> Ok Keyword
        | _ -> Error $"Unknown value kind: {kind}"

    /// Parse an IRValue from an XML element (arg or prop)
    let private parseValue (elem: XElement) : Result<IRValue, string> =
        let kAttr = elem.Attribute(xname "k")
        let vAttr = elem.Attribute(xname "v")
        let tAttr = elem.Attribute(xname "t")

        if isNull kAttr then
            Error "arg/prop must include k attribute"
        else
            match parseValueKind kAttr.Value with
            | Error e -> Error e
            | Ok kind ->
                let typeAnnotation = if isNull tAttr then None else Some tAttr.Value

                if isNull vAttr then
                    Error "arg/prop must include v attribute"
                else
                    let valueStr = vAttr.Value

                    let parseResult =
                        match kind with
                        | String -> Ok(box valueStr)
                        | Number ->
                            match Double.TryParse(valueStr) with
                            | (true, n) -> Ok(box n)
                            | (false, _) -> Error $"Cannot parse number: {valueStr}"
                        | Bool ->
                            match Boolean.TryParse(valueStr) with
                            | (true, b) -> Ok(box b)
                            | (false, _) -> Error $"Cannot parse bool: {valueStr}"
                        | Null -> Ok null
                        | Keyword -> Ok(box valueStr)

                    match parseResult with
                    | Ok v ->
                        Ok
                            { TypeAnnotation = typeAnnotation
                              Kind = kind
                              Value = v }
                    | Error e -> Error e

    /// Parse an IRNode from a <node> element
    let rec private parseNode (elem: XElement) : Result<IRNode, string> =
        let nameAttr = elem.Attribute(xname "name")

        if isNull nameAttr then
            Error "node element must have name attribute"
        else
            let name = nameAttr.Value
            let typeAttr = elem.Attribute(xname "type")
            let nodeType = if isNull typeAttr then None else Some typeAttr.Value

            // Parse args
            let argsElement = elem.Element(xname "args")

            let argsResult =
                if isNull argsElement then
                    Ok []
                else
                    argsElement.Elements(xname "arg")
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

            // Parse props
            let propsElement = elem.Element(xname "props")

            let propsResult =
                if isNull propsElement then
                    Ok Map.empty
                else
                    propsElement.Elements(xname "prop")
                    |> Seq.map (fun propElem ->
                        let keyAttr = propElem.Attribute(xname "key")

                        if isNull keyAttr then
                            Error "prop element must have key attribute"
                        else
                            match parseValue propElem with
                            | Ok v -> Ok(keyAttr.Value, v)
                            | Error e -> Error e)
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok map, Ok(k, v) -> Ok(Map.add k v map)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok Map.empty)

            // Parse children
            let childrenElement = elem.Element(xname "children")

            let childrenResult =
                if isNull childrenElement then
                    Ok []
                else
                    childrenElement.Elements(xname "node")
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

    /// Parse XML string into IR document
    let parseXML (xml: string) : Result<IRDocument, string> =
        try
            let doc = XDocument.Parse(xml)
            let root = doc.Root

            if isNull root || root.Name.LocalName <> "doc" then
                Error "document must be rooted at <doc>"
            else
                root.Elements(xname "node")
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
            Error $"XML parse error: {ex.Message}"

    /// Emit a value kind as string
    let private emitValueKind (kind: ValueKind) : string =
        match kind with
        | String -> "string"
        | Number -> "number"
        | Bool -> "bool"
        | Null -> "null"
        | Keyword -> "kw"

    /// Emit an IRValue as an <arg> element
    let private emitArg (value: IRValue) : XElement =
        let elem = XElement(xname "arg")
        elem.SetAttributeValue(xname "k", emitValueKind value.Kind)

        let valueStr =
            match value.Kind with
            | String -> value.Value :?> string
            | Number ->
                let n = value.Value :?> float
                n.ToString("G")
            | Bool ->
                let b = value.Value :?> bool
                if b then "true" else "false"
            | Null -> "null"
            | Keyword -> value.Value :?> string

        elem.SetAttributeValue(xname "v", valueStr)

        match value.TypeAnnotation with
        | Some t -> elem.SetAttributeValue(xname "t", t)
        | None -> ()

        elem

    /// Emit an IRValue as a <prop> element with key
    let private emitProp (key: string) (value: IRValue) : XElement =
        let elem = XElement(xname "prop")
        elem.SetAttributeValue(xname "key", key)
        elem.SetAttributeValue(xname "k", emitValueKind value.Kind)

        let valueStr =
            match value.Kind with
            | String -> value.Value :?> string
            | Number ->
                let n = value.Value :?> float
                n.ToString("G")
            | Bool ->
                let b = value.Value :?> bool
                if b then "true" else "false"
            | Null -> "null"
            | Keyword -> value.Value :?> string

        elem.SetAttributeValue(xname "v", valueStr)

        match value.TypeAnnotation with
        | Some t -> elem.SetAttributeValue(xname "t", t)
        | None -> ()

        elem

    /// Emit an IRNode as a <node> element
    let rec private emitNode (node: IRNode) : XElement =
        let elem = XElement(xname "node")
        elem.SetAttributeValue(xname "name", node.Name)

        match node.Type with
        | Some t -> elem.SetAttributeValue(xname "type", t)
        | None -> ()

        // Add args if present
        if not node.Arguments.IsEmpty then
            let argsElem = XElement(xname "args")
            node.Arguments |> List.iter (fun arg -> argsElem.Add(emitArg arg))
            elem.Add(argsElem)

        // Add props if present
        if not node.Properties.IsEmpty then
            let propsElem = XElement(xname "props")
            node.Properties |> Map.iter (fun key value -> propsElem.Add(emitProp key value))
            elem.Add(propsElem)

        // Add children if present
        if not node.Children.IsEmpty then
            let childrenElem = XElement(xname "children")
            node.Children |> List.iter (fun child -> childrenElem.Add(emitNode child))
            elem.Add(childrenElem)

        elem

    /// Emit IR document as XML string
    let emitXML (ir: IRDocument) : string =
        let doc = XDocument()
        let root = XElement(xname "doc")

        ir |> List.iter (fun node -> root.Add(emitNode node))

        doc.Add(root)
        doc.ToString()
