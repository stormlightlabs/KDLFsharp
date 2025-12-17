namespace KDLFSharp.Core

open System
open System.Text

/// Pretty-printer for KDL documents.
module Render =
    let private indent level = String.replicate (level * 2) " "

    let private escapeString (value: string) =
        let sb = StringBuilder()
        sb.Append('"') |> ignore

        value
        |> Seq.iter (fun ch ->
            match ch with
            | '\\' -> sb.Append(@"\\") |> ignore
            | '\"' -> sb.Append("\\\"") |> ignore
            | '\n' -> sb.Append(@"\n") |> ignore
            | '\r' -> sb.Append(@"\r") |> ignore
            | '\t' -> sb.Append(@"\t") |> ignore
            | _ -> sb.Append(ch) |> ignore)

        sb.Append('"') |> ignore
        sb.ToString()

    let private needsQuoting (text: string) =
        let isFirstCharValid c = Char.IsLetter c || c = '_' || c = '-'

        let isValidChar c =
            Char.IsLetterOrDigit c || c = '_' || c = '-' || c = '.'

        String.IsNullOrEmpty text
        || not (isFirstCharValid text[0])
        || text |> Seq.exists (fun c -> not (isValidChar c))

    let private renderIdentifier text =
        if needsQuoting text then escapeString text else text

    let private applyType ty literal =
        match ty with
        | Some typeName -> $"({typeName}){literal}"
        | None -> literal

    let rec private renderValue (value: Value) =
        match value with
        | Value.String(text, ty) -> text |> escapeString |> applyType ty
        | Value.Number(literal, ty) -> literal.Raw |> applyType ty
        | Value.Boolean(true, ty) -> applyType ty "#true"
        | Value.Boolean(false, ty) -> applyType ty "#false"
        | Value.Null ty -> applyType ty "#null"
        | Value.NodeValue _ -> "<node value>"

    let rec private renderNode (sb: StringBuilder) level (node: Node) =
        sb.Append(indent level) |> ignore

        match node.TypeAnn with
        | Some ty -> sb.Append("(").Append(ty).Append(")") |> ignore
        | None -> ()

        sb.Append(renderIdentifier node.Name) |> ignore

        node.Arguments
        |> List.iter (fun arg ->
            sb.Append(' ') |> ignore
            sb.Append(renderValue arg) |> ignore)

        node.Properties
        |> List.iter (fun prop ->
            sb.Append(' ') |> ignore

            sb.Append(renderIdentifier prop.Key).Append('=').Append(renderValue prop.Value)
            |> ignore)

        if List.isEmpty node.Children then
            sb.AppendLine() |> ignore
        else
            sb.Append(" {").AppendLine() |> ignore
            node.Children |> List.iter (renderNode sb (level + 1))
            sb.Append(indent level).Append('}').AppendLine() |> ignore

    /// Render a KDL document as formatted text.
    let renderDocument (document: Document) =
        let sb = StringBuilder()
        document |> List.iter (renderNode sb 0)
        sb.ToString()
