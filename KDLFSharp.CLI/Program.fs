open System
open System.IO
open System.Text
open KDLFSharp.Core

Console.OutputEncoding <- Encoding.UTF8

module Style =
    let inline scoped color f =
        let prev = Console.ForegroundColor
        Console.ForegroundColor <- color

        try
            f ()
        finally
            Console.ForegroundColor <- prev

    let write color text =
        scoped color (fun () -> printf "%s" text)

    let writeLine color text =
        scoped color (fun () -> printfn "%s" text)

    let dim text = write ConsoleColor.DarkGray text
    let dimLine text = writeLine ConsoleColor.DarkGray text

    let connector prefix branch =
        scoped ConsoleColor.DarkGray (fun () -> printf "%s%s" prefix branch)

    let severity color label message =
        write color $"{label}: "
        printfn "%s" message

    let info message =
        severity ConsoleColor.Green "info" message

    let warn message =
        severity ConsoleColor.Yellow "note" message

    let error message =
        severity ConsoleColor.Red "error" message

    let typeAnnotation tyOpt =
        match tyOpt with
        | Some ty ->
            dim " :"
            write ConsoleColor.DarkYellow ty
        | None -> ()

let branch isLast = if isLast then "└── " else "├── "

let nextPrefix prefix isLast =
    prefix + (if isLast then "    " else "│   ")

let rec writeValue value =
    match value with
    | Value.String(s, ty) ->
        Style.write ConsoleColor.Green $"\"{s}\""
        Style.typeAnnotation ty
    | Value.Number(lit, ty) ->
        Style.write ConsoleColor.Magenta lit.Raw
        Style.typeAnnotation ty
    | Value.Boolean true -> Style.write ConsoleColor.Yellow "#true"
    | Value.Boolean false -> Style.write ConsoleColor.Yellow "#false"
    | Value.Null -> Style.write ConsoleColor.DarkYellow "#null"
    | Value.NodeValue(nodes, ty) ->
        Style.write ConsoleColor.Cyan $"<node block x{nodes.Length}>"
        Style.typeAnnotation ty

let writeValueEntry prefix isLast label value =
    Style.connector prefix (branch isLast)
    Style.dim $"{label}: "
    writeValue value
    printfn ""

let writePropertyEntry prefix isLast (prop: Property) =
    Style.connector prefix (branch isLast)
    Style.dim "prop "
    Style.write ConsoleColor.Yellow prop.Key
    Style.dim " = "
    writeValue prop.Value
    printfn ""

let rec writeNode prefix isLast (node: Node) =
    Style.connector prefix (branch isLast)
    Style.dim "node "
    Style.write ConsoleColor.Cyan node.Name
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

let readInput argv =
    match argv |> List.ofArray with
    | [] when Console.IsInputRedirected -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | [] -> Error "no input path provided"
    | "-" :: _ -> Ok(Console.In.ReadToEnd(), "<stdin>")
    | path :: _ when File.Exists path -> Ok(File.ReadAllText path, path)
    | path :: _ -> Error $"file not found: {path}"

[<EntryPoint>]
let main argv =
    match readInput argv with
    | Error msg ->
        Style.error msg
        printfn "Usage: kdlfsharp-cli <path-to.kdl> | - (for stdin)"
        1
    | Ok(text, source) ->
        match Parser.parse text with
        | Ok nodes ->
            printDocument source nodes
            0
        | Error errs ->
            printErrors source errs
            1
