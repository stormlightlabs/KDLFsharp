namespace KDLFSharp.Core

open System
open System.Xml.Linq

/// Structured XML serialization for KDL documents (non-IR representation).
module StructuredXML =
    let private xname name = XName.Get(name)

    let private emitValue (value: Value) =
        let elem = XElement(xname "value")

        match value with
        | Value.String(text, ty) ->
            elem.SetAttributeValue(xname "kind", "string")
            elem.Value <- text

            match ty with
            | Some t -> elem.SetAttributeValue(xname "type", t)
            | None -> ()
        | Value.Number(literal, ty) ->
            elem.SetAttributeValue(xname "kind", "number")
            elem.SetAttributeValue(xname "literal", literal.Raw)

            match ty with
            | Some t -> elem.SetAttributeValue(xname "type", t)
            | None -> ()
        | Value.Boolean(b, ty) ->
            elem.SetAttributeValue(xname "kind", "bool")
            elem.Value <- if b then "true" else "false"

            match ty with
            | Some t -> elem.SetAttributeValue(xname "type", t)
            | None -> ()
        | Value.Null ty ->
            elem.SetAttributeValue(xname "kind", "null")

            match ty with
            | Some t -> elem.SetAttributeValue(xname "type", t)
            | None -> ()
        | Value.NodeValue _ -> raise (NotSupportedException("Node values are not supported in structured XML."))

        elem

    let rec private emitNode (node: Node) : XElement =
        let elem = XElement(xname "node")
        elem.SetAttributeValue(xname "name", node.Name)

        match node.TypeAnn with
        | Some ty -> elem.SetAttributeValue(xname "type", ty)
        | None -> ()

        if not node.Arguments.IsEmpty then
            let argsElem = XElement(xname "arguments")
            node.Arguments |> List.iter (fun arg -> argsElem.Add(emitValue arg))
            elem.Add(argsElem)

        if not node.Properties.IsEmpty then
            let propsElem = XElement(xname "properties")

            node.Properties
            |> List.iter (fun prop ->
                let propElem = XElement(xname "property")
                propElem.SetAttributeValue(xname "key", prop.Key)
                propElem.Add(emitValue prop.Value)
                propsElem.Add(propElem))

            elem.Add(propsElem)

        if not node.Children.IsEmpty then
            let childrenElem = XElement(xname "children")
            node.Children |> List.iter (fun child -> childrenElem.Add(emitNode child))
            elem.Add(childrenElem)

        elem

    /// Emit structured XML for a document.
    let emitDocument (document: Document) =
        let root = XElement(xname "kdl")

        for node in document do
            root.Add([| emitNode node :> obj |])

        let doc = XDocument()
        doc.Add([| root :> obj |])
        doc.ToString()

    let private parseValue (elem: XElement) =
        let kindAttr = elem.Attribute(xname "kind")

        if isNull kindAttr then
            Error "value missing kind attribute"
        else
            let tyAttr = elem.Attribute(xname "type")
            let ty = if isNull tyAttr then None else Some tyAttr.Value

            match kindAttr.Value with
            | "string" -> Ok(Value.String(elem.Value, ty))
            | "number" ->
                let literalAttr = elem.Attribute(xname "literal")

                if isNull literalAttr then
                    Error "number value missing literal attribute"
                else
                    let literal = literalAttr.Value

                    let numberLiteral =
                        match literal with
                        | "#inf" -> NumberLiteral.special literal SpecialNumberKind.Infinity
                        | "#-inf" -> NumberLiteral.special literal SpecialNumberKind.NegativeInfinity
                        | "#nan" -> NumberLiteral.special literal SpecialNumberKind.NotANumber
                        | _ -> NumberLiteral.ofRaw literal

                    Ok(Value.Number(numberLiteral, ty))
            | "bool" ->
                let text = elem.Value.Trim()

                match Boolean.TryParse(text) with
                | true, b -> Ok(Value.Boolean(b, ty))
                | _ -> Error $"Invalid bool value: {text}"
            | "null" -> Ok(Value.Null ty)
            | other -> Error $"Unsupported value kind: {other}"

    let rec private parseNode (elem: XElement) =
        let nameAttr = elem.Attribute(xname "name")

        if isNull nameAttr then
            Error "node missing name attribute"
        else
            let typeAttr = elem.Attribute(xname "type")
            let ty = if isNull typeAttr then None else Some typeAttr.Value

            let argsResult =
                let argsElement = elem.Element(xname "arguments")

                if isNull argsElement then
                    Ok []
                else
                    argsElement.Elements(xname "value")
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
                let propsElement = elem.Element(xname "properties")

                if isNull propsElement then
                    Ok []
                else
                    propsElement.Elements(xname "property")
                    |> Seq.map (fun propElem ->
                        let keyAttr = propElem.Attribute(xname "key")

                        if isNull keyAttr then
                            Error "property missing key attribute"
                        else
                            match propElem.Element(xname "value") with
                            | null -> Error "property missing value element"
                            | valueElem ->
                                match parseValue valueElem with
                                | Ok v -> Ok({ Key = keyAttr.Value; Value = v })
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

            let childrenResult =
                let childrenElement = elem.Element(xname "children")

                if isNull childrenElement then
                    Ok []
                else
                    childrenElement.Elements(xname "node")
                    |> Seq.map parseNode
                    |> Seq.toList
                    |> List.fold
                        (fun acc r ->
                            match acc, r with
                            | Ok lst, Ok node -> Ok(node :: lst)
                            | Error e, _ -> Error e
                            | _, Error e -> Error e)
                        (Ok [])
                    |> Result.map List.rev

            match argsResult, propsResult, childrenResult with
            | Ok args, Ok props, Ok children ->
                Ok
                    { TypeAnn = ty
                      Name = nameAttr.Value
                      Arguments = args
                      Properties = props
                      Children = children
                      Span = None
                      OriginalText = None }
            | Error e, _, _ -> Error e
            | _, Error e, _ -> Error e
            | _, _, Error e -> Error e

    /// Parse structured XML into a document.
    let parseDocument (xml: string) : Result<Document, string> =
        try
            let doc = XDocument.Parse(xml)
            let root = doc.Root

            if isNull root || root.Name <> xname "kdl" then
                Error "document must be rooted at <kdl>"
            else
                root.Elements(xname "node")
                |> Seq.map parseNode
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
            Error $"XML parse error: {ex.Message}"
