open System
open System.IO
open System.Text
open KDLFSharp.Core

Console.OutputEncoding <- Encoding.UTF8

module Style =
    module Palette =
        let azureDark = (55, 139, 186)
        let azureLight = (48, 185, 219)
        let lightRed = (255, 179, 179)
        let lightAzure = (192, 241, 255)
        let lightLavender = (236, 214, 255)
        let purple = (140, 82, 255)
        let dim = (110, 110, 110)

    let private render (r, g, b) text =
        $"\u001b[38;2;{r};{g};{b}m{text}\u001b[0m"

    let write color text = Console.Write(render color text)

    let writeLine color text = Console.WriteLine(render color text)

    let dim text = write Palette.dim text
    let dimLine text = writeLine Palette.dim text

    let connector prefix branch = write Palette.dim $"{prefix}{branch}"

    let severity color label message = writeLine color $"{label}: {message}"

    let info message =
        severity Palette.azureLight "info" message

    let warn message = severity Palette.purple "note" message

    let error message =
        severity Palette.lightRed "error" message

    let typeAnnotation tyOpt =
        match tyOpt with
        | Some ty ->
            dim " :"
            write Palette.lightLavender ty
        | None -> ()

let branch isLast = if isLast then "└── " else "├── "

let nextPrefix prefix isLast =
    prefix + (if isLast then "    " else "│   ")

let rec writeValue value =
    match value with
    | Value.String(s, ty) ->
        Style.write Style.Palette.lightAzure $"\"{s}\""
        Style.typeAnnotation ty
    | Value.Number(lit, ty) ->
        Style.write Style.Palette.purple lit.Raw
        Style.typeAnnotation ty
    | Value.Boolean(true, ty) ->
        Style.write Style.Palette.azureLight "#true"
        Style.typeAnnotation ty
    | Value.Boolean(false, ty) ->
        Style.write Style.Palette.azureLight "#false"
        Style.typeAnnotation ty
    | Value.Null ty ->
        Style.write Style.Palette.lightRed "#null"
        Style.typeAnnotation ty
    | Value.NodeValue(nodes, ty) ->
        Style.write Style.Palette.azureDark $"<node block x{nodes.Length}>"
        Style.typeAnnotation ty

let writeValueEntry prefix isLast label value =
    Style.connector prefix (branch isLast)
    Style.dim $"{label}: "
    writeValue value
    printfn ""

let writePropertyEntry prefix isLast (prop: Property) =
    Style.connector prefix (branch isLast)
    Style.dim "prop "
    Style.write Style.Palette.azureDark prop.Key
    Style.dim " = "
    writeValue prop.Value
    printfn ""

let rec writeNode prefix isLast (node: Node) =
    Style.connector prefix (branch isLast)
    Style.dim "node "
    Style.write Style.Palette.azureDark node.Name
    Style.typeAnnotation node.TypeAnn

    if not (List.isEmpty node.Arguments) then
        Style.dim $"  [{List.length node.Arguments} arg]"

    if not (List.isEmpty node.Properties) then
        Style.dim $"  [{List.length node.Properties} prop]"

    if not (List.isEmpty node.Children) then
        Style.dim $"  [{List.length node.Children} child]"

    printfn ""

    let entries =
        [ yield! node.Arguments |> List.mapi (fun i v -> Choice1Of3(i, v))
          yield! node.Properties |> List.map Choice2Of3
          yield! node.Children |> List.map Choice3Of3 ]

    let pref = nextPrefix prefix isLast

    let entryCount = List.length entries

    entries
    |> List.iteri (fun idx entry ->
        let isLastEntry = idx = entryCount - 1

        match entry with
        | Choice1Of3(i, value) -> writeValueEntry pref isLastEntry $"arg[{i}]" value
        | Choice2Of3 prop -> writePropertyEntry pref isLastEntry prop
        | Choice3Of3 child -> writeNode pref isLastEntry child)

let printDocument source nodes =
    Style.info $"parsed {List.length nodes} node(s) from {source}"

    if List.isEmpty nodes then
        Style.dimLine "  (document is empty)"
    else
        Style.dimLine $"  ── tree for {source}"

        nodes
        |> List.iteri (fun idx node ->
            let isLast = idx = List.length nodes - 1
            writeNode "" isLast node)

let printErrors source errs =
    errs
    |> List.iteri (fun idx err ->
        Style.error err.Message
        Style.dim "  --> "
        printfn "%s" (err.SourceName |> Option.defaultValue source)

        err.Context |> Option.iter (fun ctx -> Style.warn ctx))

type ConversionKind =
    | KdlToJson
    | JsonToKdl
    | KdlToXml
    | XmlToKdl

type OutputTarget =
    | Stdout
    | File of string

type ConversionCommand =
    { Kind: ConversionKind
      Input: string
      Target: OutputTarget
      DebugIR: bool }

let printUsage () =
    printfn "Usage:"
    printfn "  kdlfsharp-cli <path-to.kdl> | - (prints AST tree)"
    printfn "  kdlfsharp-cli kdl-to-json <path|- > [--out <file>] [--debug]"
    printfn "  kdlfsharp-cli json-to-kdl <path|- > [--out <file>] [--debug]"
    printfn "  kdlfsharp-cli kdl-to-xml <path|- > [--out <file>] [--debug]"
    printfn "  kdlfsharp-cli xml-to-kdl <path|- > [--out <file>] [--debug]"
    printfn ""
    printfn "  --debug emits/consumes the canonical IR format; without it, structured JSON/XML is used."

let readSourceArg arg =
    match arg with
    | "-" -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | path when File.Exists path -> Ok(File.ReadAllText path, path)
    | path -> Error $"file not found: {path}"

let writeOutput target (content: string) =
    match target with
    | Stdout -> Console.WriteLine(content)
    | File path ->
        File.WriteAllText(path, content)
        Style.info $"wrote output to {path}"

let runKdlToJson (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        match Parser.parse text with
        | Error errs ->
            printErrors source errs
            1
        | Ok nodes ->
            if cmd.DebugIR then
                match DocumentConversion.toIR nodes with
                | Error err ->
                    Style.error err
                    1
                | Ok ir ->
                    let output = JSON.emitJSON ir
                    writeOutput cmd.Target output
                    0
            else
                let output = StructuredJSON.emitDocument nodes
                writeOutput cmd.Target output
                0

let runKdlToXml (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        match Parser.parse text with
        | Error errs ->
            printErrors source errs
            1
        | Ok nodes ->
            if cmd.DebugIR then
                match DocumentConversion.toIR nodes with
                | Error err ->
                    Style.error err
                    1
                | Ok ir ->
                    let output = XML.emitXML ir
                    writeOutput cmd.Target output
                    0
            else
                let output = StructuredXML.emitDocument nodes
                writeOutput cmd.Target output
                0

let runJsonToKdl (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        let docResult =
            if cmd.DebugIR then
                JSON.parseJSON text |> Result.bind DocumentConversion.ofIR
            else
                StructuredJSON.parseDocument text

        match docResult with
        | Error err ->
            Style.error $"failed to parse json: {err}"
            Style.dimLine $"  source: {source}"
            1
        | Ok doc ->
            let rendered = Render.renderDocument doc
            writeOutput cmd.Target rendered
            0

let runXmlToKdl (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        let docResult =
            if cmd.DebugIR then
                XML.parseXML text |> Result.bind DocumentConversion.ofIR
            else
                StructuredXML.parseDocument text

        match docResult with
        | Error err ->
            Style.error $"failed to parse xml: {err}"
            Style.dimLine $"  source: {source}"
            1
        | Ok doc ->
            let rendered = Render.renderDocument doc
            writeOutput cmd.Target rendered
            0

let tryParseConversionCommand (args: string list) =
    let rec parseArgs input out debug remaining =
        match remaining with
        | [] ->
            match input with
            | Some inp -> Ok(inp, out, debug)
            | None when Console.IsInputRedirected -> Ok("-", out, debug)
            | None -> Error "conversion commands require an input path or '-' for stdin"
        | "--out" :: path :: tail -> parseArgs input (Some path) debug tail
        | "--out" :: [] -> Error "--out flag requires a path"
        | "--debug" :: tail -> parseArgs input out true tail
        | opt :: _ when opt.StartsWith("--") -> Error $"unknown option: {opt}"
        | value :: tail ->
            match input with
            | None -> parseArgs (Some value) out debug tail
            | Some _ -> Error $"unexpected argument: {value}"

    match args with
    | command :: rest ->
        let kindOpt =
            match command with
            | "kdl-to-json" -> Some KdlToJson
            | "json-to-kdl" -> Some JsonToKdl
            | "kdl-to-xml" -> Some KdlToXml
            | "xml-to-kdl" -> Some XmlToKdl
            | _ -> None

        kindOpt
        |> Option.map (fun kind ->
            parseArgs None None false rest
            |> Result.map (fun (input, outOpt, debug) ->
                let target = outOpt |> Option.map File |> Option.defaultValue Stdout

                { Kind = kind
                  Input = input
                  Target = target
                  DebugIR = debug }))
    | [] -> None

let readInput argv =
    match argv |> List.ofArray with
    | [] when Console.IsInputRedirected -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | [] -> Error "no input path provided"
    | "-" :: _ -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | path :: _ when File.Exists path -> Ok(File.ReadAllText path, path)
    | path :: _ -> Error $"file not found: {path}"

[<EntryPoint>]
let main argv =
    let args = argv |> List.ofArray

    match tryParseConversionCommand args with
    | Some(Ok cmd) ->
        match cmd.Kind with
        | KdlToJson -> runKdlToJson cmd
        | KdlToXml -> runKdlToXml cmd
        | JsonToKdl -> runJsonToKdl cmd
        | XmlToKdl -> runXmlToKdl cmd
    | Some(Error msg) ->
        Style.error msg
        printUsage ()
        1
    | None ->
        match readInput argv with
        | Error msg ->
            Style.error msg
            printUsage ()
            1
        | Ok(text, source) ->
            match Parser.parse text with
            | Ok nodes ->
                printDocument source nodes
                0
            | Error errs ->
                printErrors source errs
                1
