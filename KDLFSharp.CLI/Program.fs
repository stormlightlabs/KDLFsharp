open System
open System.IO
open System.Text
open KDLFSharp.Core

Console.OutputEncoding <- Encoding.UTF8

type ConversionKind =
    | KdlToJson
    | JsonToKdl
    | KdlToXml
    | XmlToKdl

type OutputTarget =
    | Stdout
    | File of string

type OutputMode =
    | Structured
    | Debug
    | Sample

type ConversionCommand =
    { Kind: ConversionKind
      Input: string
      Target: OutputTarget
      Mode: OutputMode
      Pretty: bool
      Lossy: bool
      SampleNamespace: string option }

module Style =
    module Palette =
        let azureDark = 55, 139, 186
        let azureLight = 48, 185, 219
        let lightRed = 255, 179, 179
        let lightAzure = 192, 241, 255
        let lightLavender = 236, 214, 255
        let purple = 140, 82, 255
        let dim = 110, 110, 110

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
    prefix + if isLast then "    " else "│   "

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



let printUsage () =
    Style.write Style.Palette.lightAzure "KDL "
    Style.write Style.Palette.azureDark "F# "
    Style.write Style.Palette.lightLavender "CLI"
    Style.write Style.Palette.dim ":"
    Style.write Style.Palette.lightRed " Parse, display, and convert to & from KDL"
    printfn ""
    printfn ""
    Style.writeLine Style.Palette.azureLight "USAGE:"
    Style.write Style.Palette.purple "    kdlfsharp-cli "
    Style.write Style.Palette.lightLavender "<command> <input> "
    Style.writeLine Style.Palette.dim "[options]"
    printfn ""
    Style.writeLine Style.Palette.azureLight "COMMANDS:"
    Style.write Style.Palette.purple "    kdl-to-json    "
    Style.dimLine "Convert KDL to JSON"
    Style.write Style.Palette.purple "    json-to-kdl    "
    Style.dimLine "Convert JSON to KDL"
    Style.write Style.Palette.purple "    kdl-to-xml     "
    Style.dimLine "Convert KDL to XML"
    Style.write Style.Palette.purple "    xml-to-kdl     "
    Style.dimLine "Convert XML to KDL"
    printfn ""
    Style.writeLine Style.Palette.azureLight "INPUT:"
    Style.write Style.Palette.lightLavender "    <path>         "
    Style.dimLine "Path to input file"
    Style.write Style.Palette.lightLavender "    -              "
    Style.dimLine "Read from stdin"
    printfn ""
    Style.writeLine Style.Palette.azureLight "OPTIONS:"
    Style.write Style.Palette.lightAzure "    --out "
    Style.write Style.Palette.lightLavender "<file>       "
    Style.dimLine "Write output to file (default: stdout)"
    Style.write Style.Palette.lightAzure "    --pretty           "
    Style.dimLine "Enable colored output to stdout"
    Style.write Style.Palette.lightAzure "    --debug            "
    Style.dimLine "Use canonical IR schema (lossless round-trips)"
    Style.write Style.Palette.lightAzure "    --sample --lossy   "
    Style.dimLine "Use friendly sample format (lossy, for human reading)"
    Style.write Style.Palette.lightAzure "    --sample-ns "
    Style.write Style.Palette.lightLavender "<uri>  "
    Style.dimLine "Override XML metadata namespace (sample mode only)"
    printfn ""
    Style.writeLine Style.Palette.azureLight "OUTPUT MODES:"
    Style.write Style.Palette.purple "    Structured "
    Style.write Style.Palette.lightAzure "(default) "
    Style.dimLine "- AST with explicit types and annotations"
    Style.write Style.Palette.purple "    Debug "
    Style.write Style.Palette.lightAzure "(--debug)      "
    Style.dimLine "- Canonical IR for lossless round-trips"
    Style.write Style.Palette.purple "    Sample "
    Style.write Style.Palette.lightAzure "(--sample)    "
    Style.dimLine "- Human-friendly format with metadata in _meta blocks"
    printfn ""
    Style.writeLine Style.Palette.azureLight "EXAMPLES:"
    Style.write Style.Palette.dim "    "
    Style.write Style.Palette.purple "kdlfsharp-cli "
    Style.writeLine Style.Palette.lightLavender "data/sample.kdl"
    Style.write Style.Palette.dim "    "
    Style.write Style.Palette.purple "kdlfsharp-cli "
    Style.write Style.Palette.azureDark "kdl-to-json "
    Style.write Style.Palette.lightLavender "data/sample.kdl "
    Style.write Style.Palette.lightAzure "--out "
    Style.writeLine Style.Palette.lightLavender "output.json"
    Style.write Style.Palette.dim "    "
    Style.write Style.Palette.purple "kdlfsharp-cli "
    Style.write Style.Palette.azureDark "json-to-kdl "
    Style.write Style.Palette.lightLavender "input.json "
    Style.writeLine Style.Palette.lightAzure "--pretty"
    Style.write Style.Palette.dim "    "
    Style.write Style.Palette.purple "kdlfsharp-cli "
    Style.write Style.Palette.azureDark "kdl-to-xml "
    Style.write Style.Palette.lightLavender "data/sample.kdl "
    Style.writeLine Style.Palette.lightAzure "--sample --lossy"
    Style.write Style.Palette.dim "    "
    Style.write Style.Palette.purple "cat "
    Style.write Style.Palette.lightAzure "data/sample.kdl "
    Style.write Style.Palette.dim "| "
    Style.write Style.Palette.purple "kdlfsharp-cli "
    Style.writeLine Style.Palette.lightLavender "-"
    printfn ""

let readSourceArg arg =
    match arg with
    | "-" -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | path when File.Exists path -> Ok(File.ReadAllText path, path)
    | path -> Error $"file not found: {path}"

let writeOutput target pretty color (content: string) =
    match target with
    | Stdout when pretty -> Style.writeLine color content
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
            match cmd.Mode with
            | Debug ->
                match DocumentConversion.toIR nodes with
                | Error err ->
                    Style.error err
                    1
                | Ok ir ->
                    let output = JSON.emitJSON ir
                    writeOutput cmd.Target cmd.Pretty Style.Palette.azureLight output
                    0
            | Structured ->
                let output = StructuredJSON.emitDocument nodes
                writeOutput cmd.Target cmd.Pretty Style.Palette.azureLight output
                0
            | Sample ->
                if not cmd.Lossy then
                    Style.error "--sample output requires --lossy acknowledgement"
                    1
                else
                    Style.warn "sample output is lossy; use --debug for canonical round-trips"
                    let output = SampleJSON.emitDocument nodes
                    writeOutput cmd.Target cmd.Pretty Style.Palette.azureLight output
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
            match cmd.Mode with
            | Debug ->
                match DocumentConversion.toIR nodes with
                | Error err ->
                    Style.error err
                    1
                | Ok ir ->
                    let output = XML.emitXML ir
                    writeOutput cmd.Target cmd.Pretty Style.Palette.lightLavender output
                    0
            | Structured ->
                let output = StructuredXML.emitDocument nodes
                writeOutput cmd.Target cmd.Pretty Style.Palette.lightLavender output
                0
            | Sample ->
                if not cmd.Lossy then
                    Style.error "--sample output requires --lossy acknowledgement"
                    1
                else
                    Style.warn "sample output is lossy; use --debug for canonical round-trips"
                    let ns = cmd.SampleNamespace |> Option.defaultValue SampleXML.defaultNamespace
                    let output = SampleXML.emitDocumentWithNamespace ns nodes
                    writeOutput cmd.Target cmd.Pretty Style.Palette.lightLavender output
                    0

let runJsonToKdl (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        match cmd.Mode with
        | Sample ->
            match SampleJSON.parseDocument text with
            | Error err ->
                Style.error $"failed to parse sample json: {err}"
                Style.dimLine $"  source: {source}"
                1
            | Ok doc ->
                let rendered = Render.renderDocument doc
                writeOutput cmd.Target cmd.Pretty Style.Palette.azureDark rendered
                0
        | Debug
        | Structured as mode ->
            let docResult =
                match mode with
                | Debug -> JSON.parseJSON text |> Result.bind DocumentConversion.ofIR
                | Structured -> StructuredJSON.parseDocument text
                | Sample -> failwith "unreachable"

            match docResult with
            | Error err ->
                Style.error $"failed to parse json: {err}"
                Style.dimLine $"  source: {source}"
                1
            | Ok doc ->
                let rendered = Render.renderDocument doc
                writeOutput cmd.Target cmd.Pretty Style.Palette.azureDark rendered
                0

let runXmlToKdl (cmd: ConversionCommand) =
    match readSourceArg cmd.Input with
    | Error msg ->
        Style.error msg
        1
    | Ok(text, source) ->
        match cmd.Mode with
        | Sample ->
            match SampleXML.parseDocument text with
            | Error err ->
                Style.error $"failed to parse sample xml: {err}"
                Style.dimLine $"  source: {source}"
                1
            | Ok doc ->
                let rendered = Render.renderDocument doc
                writeOutput cmd.Target cmd.Pretty Style.Palette.lightLavender rendered
                0
        | Debug
        | Structured as mode ->
            let docResult =
                match mode with
                | Debug -> XML.parseXML text |> Result.bind DocumentConversion.ofIR
                | Structured -> StructuredXML.parseDocument text
                | Sample -> failwith "unreachable"

            match docResult with
            | Error err ->
                Style.error $"failed to parse xml: {err}"
                Style.dimLine $"  source: {source}"
                1
            | Ok doc ->
                let rendered = Render.renderDocument doc
                writeOutput cmd.Target cmd.Pretty Style.Palette.lightLavender rendered
                0

let tryParseConversionCommand (args: string list) =
    let rec parseArgs input out mode lossy sampleNs pretty remaining =
        match remaining with
        | [] ->
            match input with
            | Some inp -> Ok(inp, out, mode, lossy, sampleNs, pretty)
            | None when Console.IsInputRedirected -> Ok("-", out, mode, lossy, sampleNs, pretty)
            | None -> Error "conversion commands require an input path or '-' for stdin"
        | "--out" :: path :: tail -> parseArgs input (Some path) mode lossy sampleNs pretty tail
        | "--out" :: [] -> Error "--out flag requires a path"
        | "--debug" :: tail ->
            match mode with
            | Some Sample -> Error "cannot combine --debug with --sample"
            | _ -> parseArgs input out (Some Debug) lossy sampleNs pretty tail
        | "--sample" :: tail ->
            match mode with
            | Some Debug -> Error "cannot combine --sample with --debug"
            | _ -> parseArgs input out (Some Sample) lossy sampleNs pretty tail
        | "--sample-ns" :: uri :: tail -> parseArgs input out mode lossy (Some uri) pretty tail
        | "--sample-ns" :: [] -> Error "--sample-ns flag requires a namespace URI"
        | "--lossy" :: tail -> parseArgs input out mode true sampleNs pretty tail
        | "--pretty" :: tail -> parseArgs input out mode lossy sampleNs true tail
        | opt :: _ when opt.StartsWith("--") -> Error $"unknown option: {opt}"
        | value :: tail ->
            match input with
            | None -> parseArgs (Some value) out mode lossy sampleNs pretty tail
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
            match parseArgs None None None false None false rest with
            | Error e -> Error e
            | Ok(input, outOpt, modeOpt, lossy, sampleNs, pretty) ->
                let resolvedMode = modeOpt |> Option.defaultValue Structured

                match sampleNs, resolvedMode, kind with
                | Some _, Sample, KdlToXml ->
                    Ok
                        { Kind = kind
                          Input = input
                          Target = outOpt |> Option.map File |> Option.defaultValue Stdout
                          Mode = resolvedMode
                          Pretty = pretty
                          Lossy = lossy
                          SampleNamespace = sampleNs }
                | Some _, _, KdlToXml -> Error "--sample-ns requires --sample mode"
                | Some _, _, _ -> Error "--sample-ns is only valid with kdl-to-xml"
                | None, _, _ ->
                    Ok
                        { Kind = kind
                          Input = input
                          Target = outOpt |> Option.map File |> Option.defaultValue Stdout
                          Mode = resolvedMode
                          Pretty = pretty
                          Lossy = lossy
                          SampleNamespace = None })
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

    match args with
    | [] when not Console.IsInputRedirected ->
        printUsage ()
        0
    | [ "--help" ]
    | [ "-h" ]
    | [ "help" ] ->
        printUsage ()
        0
    | _ ->
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
