namespace KDLFSharp.Core

open System
open System.Globalization
open System.Xml.Linq

/// Emits and parses a friendlier, sample-style XML representation (lossy).
module SampleXML =
    let defaultNamespace = "https://github.com/stormlightlabs/KDLFsharp"

    let emitDocumentWithNamespace namespaceUri (document: Document) =
        let metaNs = XNamespace.Get namespaceUri
        let metaName = metaNs + "meta"
        let argName = metaNs + "argument"
        let propTypeName = metaNs + "property"
        let typeName = metaNs + "type"

        let rec toText (value: Value) =
            match value with
            | Value.String(s, _) -> s
            | Value.Number(lit, _) -> lit.Raw
            | Value.Boolean(true, _) -> "true"
            | Value.Boolean(false, _) -> "false"
            | Value.Null _ -> ""
            | Value.NodeValue _ -> "<node>"

        let rec emitNode (node: Node) =
            let elem = XElement(XName.Get node.Name)

            node.Properties
            |> List.iter (fun prop -> elem.SetAttributeValue(XName.Get prop.Key, toText prop.Value))

            node.Children |> List.iter (fun child -> elem.Add(emitNode child :> obj))

            let metaElements = ResizeArray<XElement>()

            node.TypeAnn
            |> Option.iter (fun ty ->
                let tyElem = XElement(typeName)
                tyElem.Value <- ty
                metaElements.Add(tyElem))

            node.Arguments
            |> List.mapi (fun idx arg ->
                let argElem = XElement(argName)
                argElem.SetAttributeValue(XName.Get "index", idx)
                argElem.Value <- toText arg

                match arg with
                | Value.String(_, Some ty)
                | Value.Number(_, Some ty)
                | Value.Boolean(_, Some ty)
                | Value.Null(Some ty) -> argElem.SetAttributeValue(XName.Get "type", ty)
                | _ -> ()

                argElem)
            |> List.iter metaElements.Add

            node.Properties
            |> List.iter (fun prop ->
                match prop.Value with
                | Value.String(_, Some ty)
                | Value.Number(_, Some ty)
                | Value.Boolean(_, Some ty)
                | Value.Null(Some ty) ->
                    let pt = XElement(propTypeName)
                    pt.SetAttributeValue(XName.Get "name", prop.Key)
                    pt.SetAttributeValue(XName.Get "type", ty)
                    metaElements.Add(pt)
                | _ -> ())

            if metaElements.Count > 0 then
                let metaElem = XElement(metaName)
                metaElements |> Seq.iter metaElem.Add
                elem.Add(metaElem)

            elem

        let nodes = document |> List.map emitNode

        let root =
            match nodes with
            | [ single ] -> single
            | multiple ->
                let container = XElement(XName.Get "kdl")
                multiple |> List.iter (fun elem -> container.Add(elem :> obj))
                container

        let doc = XDocument()
        doc.Add(root :> obj)
        doc.ToString()

    let emitDocument document =
        emitDocumentWithNamespace defaultNamespace document

    let private parseNumberLiteral text =
        match text with
        | "#inf" -> Value.Number(NumberLiteral.special "#inf" SpecialNumberKind.Infinity, None)
        | "#-inf" -> Value.Number(NumberLiteral.special "#-inf" SpecialNumberKind.NegativeInfinity, None)
        | "#nan" -> Value.Number(NumberLiteral.special "#nan" SpecialNumberKind.NotANumber, None)
        | _ ->
            match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, _ -> Value.Number(NumberLiteral.ofRaw text, None)
            | _ -> Value.String(text, None)

    let private applyType tyOpt value =
        match value with
        | Value.String(s, _) -> Value.String(s, tyOpt)
        | Value.Number(lit, _) -> Value.Number(lit, tyOpt)
        | Value.Boolean(b, _) -> Value.Boolean(b, tyOpt)
        | Value.Null _ -> Value.Null tyOpt
        | Value.NodeValue(nodes, _) -> Value.NodeValue(nodes, tyOpt)

    let private parseScalar text tyOpt =
        let baseValue =
            match text with
            | "#true" -> Value.Boolean(true, None)
            | "#false" -> Value.Boolean(false, None)
            | "#null" -> Value.Null None
            | _ ->
                match Boolean.TryParse(text) with
                | true, b -> Value.Boolean(b, None)
                | _ -> parseNumberLiteral text

        applyType tyOpt baseValue

    let private tryGetAttribute (elem: XElement) (name: string) =
        let attr = elem.Attribute(XName.Get name)
        if isNull attr then None else Some attr.Value

    let rec private parseNodeElement (elem: XElement) : Result<Node, string> =
        let name = elem.Name.LocalName

        let metaElem = elem.Elements() |> Seq.tryFind (fun e -> e.Name.LocalName = "meta")

        let typeAnnotation =
            metaElem
            |> Option.bind (fun meta ->
                meta.Elements()
                |> Seq.tryFind (fun e -> e.Name.LocalName = "type")
                |> Option.map (fun e -> e.Value))

        let argumentEntries =
            metaElem
            |> Option.map (fun meta ->
                meta.Elements()
                |> Seq.filter (fun e -> e.Name.LocalName = "argument")
                |> Seq.choose (fun argElem ->
                    match tryGetAttribute argElem "index" with
                    | Some idxStr when Int32.TryParse idxStr |> fst ->
                        let idx = Int32.Parse idxStr
                        let ty = tryGetAttribute argElem "type"
                        Some(idx, parseScalar argElem.Value ty)
                    | _ -> None)
                |> Seq.toList)
            |> Option.defaultValue []

        let propertyTypes =
            metaElem
            |> Option.map (fun meta ->
                meta.Elements()
                |> Seq.filter (fun e -> e.Name.LocalName = "property")
                |> Seq.choose (fun propElem ->
                    match tryGetAttribute propElem "name" with
                    | Some key -> Some(key, tryGetAttribute propElem "type")
                    | None -> None)
                |> Map.ofSeq)
            |> Option.defaultValue Map.empty

        let arguments = argumentEntries |> List.sortBy fst |> List.map snd

        let properties =
            elem.Attributes()
            |> Seq.filter (fun attr -> not attr.IsNamespaceDeclaration)
            |> Seq.map (fun attr ->
                let ty = Map.tryFind attr.Name.LocalName propertyTypes |> Option.flatten

                { Key = attr.Name.LocalName
                  Value = parseScalar attr.Value ty })
            |> Seq.toList

        let childrenResult =
            elem.Elements()
            |> Seq.filter (fun child -> child.Name.LocalName <> "meta")
            |> Seq.map parseNodeElement
            |> Seq.fold
                (fun state result ->
                    match state, result with
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                    | Ok acc, Ok child -> Ok(child :: acc))
                (Ok [])

        match childrenResult with
        | Error e -> Error e
        | Ok children ->
            Ok
                { TypeAnn = typeAnnotation
                  Name = name
                  Arguments = arguments
                  Properties = properties
                  Children = List.rev children
                  Span = None
                  OriginalText = None }

    let parseDocument (xml: string) : Result<Document, string> =
        try
            let doc = XDocument.Parse(xml)
            let root = doc.Root

            if isNull root then
                Error "XML document has no root element"
            else if root.Name.LocalName = "kdl" then
                root.Elements()
                |> Seq.map parseNodeElement
                |> Seq.fold
                    (fun state result ->
                        match state, result with
                        | Error e, _ -> Error e
                        | _, Error e -> Error e
                        | Ok acc, Ok node -> Ok(node :: acc))
                    (Ok [])
                |> Result.map List.rev
            else
                parseNodeElement root |> Result.map List.singleton
        with ex ->
            Error $"XML parse error: {ex.Message}"
