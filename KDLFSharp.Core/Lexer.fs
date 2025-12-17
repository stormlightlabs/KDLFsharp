namespace KDLFSharp.Core

open System
open System.Text

/// Tokens the parser will consume.
type Token =
    | Ident of string
    | String of string
    | RawString of string
    | Number of string
    | Keyword of string
    | LBrace
    | RBrace
    | Eq
    | Semicolon
    | SlashDash
    | TypeLParen
    | TypeRParen
    | Newline
    | Eof
    | TokenError of string * int

    override this.ToString() =
        match this with
        | Ident s -> $"Ident({s})"
        | String s -> $"String({s})"
        | RawString s -> $"RawString({s})"
        | Number s -> $"Number({s})"
        | Keyword k -> $"Keyword({k})"
        | LBrace -> "{"
        | RBrace -> "}"
        | Eq -> "="
        | Semicolon -> ";"
        | SlashDash -> "/-"
        | TypeLParen -> "("
        | TypeRParen -> ")"
        | Newline -> "⏎"
        | Eof -> "<eof>"
        | TokenError(msg, pos) -> $"Error({msg} @ {pos})"

/// Mutable cursor over the source text.
type LexerState =
    { Source: string
      mutable Pos: int
      mutable Line: int
      mutable Col: int }

[<AutoOpen>]
module private LexUtil =
    let inline isDigit c = c >= '0' && c <= '9'
    let inline isAlpha c = Char.IsLetter(c) || c = '_'

    let inline isIdentChar c =
        isAlpha c || isDigit c || c = '-' || c = '_'

    let inline isHexDigit c =
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')

    let inline isOctDigit c = c >= '0' && c <= '7'
    let inline isBinDigit c = c = '0' || c = '1'
    let inline isWhitespaceChar c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

    let peek (st: LexerState) =
        if st.Pos >= st.Source.Length then
            None
        else
            Some st.Source[st.Pos]

    let peekAhead (st: LexerState) (offset: int) =
        let idx = st.Pos + offset

        if idx >= st.Source.Length then
            None
        else
            Some st.Source[idx]

    let advance (st: LexerState) =
        if st.Pos >= st.Source.Length then
            None
        else
            let c = st.Source[st.Pos]
            st.Pos <- st.Pos + 1
            st.Col <- st.Col + 1
            Some c

    let bumpLine (st: LexerState) =
        st.Line <- st.Line + 1
        st.Col <- 1

    let currentPos (st: LexerState) = st.Pos

    let isAtSequence (st: LexerState) (sequence: string) =
        let mutable idx = 0
        let mutable ok = true

        while ok && idx < sequence.Length do
            match peekAhead st idx with
            | Some c when c = sequence[idx] -> idx <- idx + 1
            | _ -> ok <- false

        ok

    let advanceMany (st: LexerState) (count: int) =
        for _ in 1..count do
            advance st |> ignore

module Lexer =
    let private consumeEscapedWhitespace (st: LexerState) =
        let rec loop () =
            match peek st with
            | Some (' ' | '\t') ->
                advance st |> ignore
                loop ()
            | Some '\r' ->
                advance st |> ignore

                match peek st with
                | Some '\n' -> advance st |> ignore
                | _ -> ()

                bumpLine st
                loop ()
            | Some '\n' ->
                advance st |> ignore
                bumpLine st
                loop ()
            | _ -> ()

        loop ()

    let private parseUnicodeEscape (st: LexerState) (startPos: int) =
        let hex = StringBuilder()

        let rec gather () =
            match advance st with
            | None -> Error $"Unterminated unicode escape starting at {startPos}"
            | Some '}' ->
                if hex.Length = 0 then
                    Error $"Empty unicode escape at {startPos}"
                else
                    Ok ()
            | Some c when isHexDigit c ->
                if hex.Length >= 6 then
                    Error $"Unicode escape too long at {startPos}"
                else
                    hex.Append(c) |> ignore
                    gather ()
            | Some _ -> Error $"Invalid unicode escape at {startPos}"

        match advance st with
        | Some '{' ->
            match gather () with
            | Error msg -> Error msg
            | Ok _ ->
                try
                    let value = Convert.ToInt32(hex.ToString(), 16)

                    if value > 0x10FFFF || (value >= 0xD800 && value <= 0xDFFF) then
                        Error $"Unicode escape at {startPos} is not a scalar value"
                    else
                        let rune = Rune(value)
                        Ok(rune.ToString())
                with _ ->
                    Error $"Invalid unicode escape at {startPos}"
        | _ -> Error $"Invalid unicode escape at {startPos}"

    let private lexQuoted (st: LexerState) (startPos: int) =
        let sb = StringBuilder()
        let mutable finished = false
        let mutable error: string option = None

        let appendEscape (ch: char) =
            sb.Append(ch) |> ignore

        let rec loop () =
            if not finished && error.IsNone then
                match advance st with
                | None -> error <- Some $"Unterminated string starting at {startPos}"
                | Some '"' -> finished <- true
                | Some '\\' ->
                    match peek st with
                    | Some c when isWhitespaceChar c ->
                        consumeEscapedWhitespace st
                        loop ()
                    | _ ->
                        match advance st with
                        | Some 'n' ->
                            appendEscape '\n'
                            loop ()
                        | Some 'r' ->
                            appendEscape '\r'
                            loop ()
                        | Some 't' ->
                            appendEscape '\t'
                            loop ()
                        | Some 'b' ->
                            appendEscape '\b'
                            loop ()
                        | Some 'f' ->
                            appendEscape '\u000C'
                            loop ()
                        | Some 's' ->
                            appendEscape ' '
                            loop ()
                        | Some '"' ->
                            appendEscape '"'
                            loop ()
                        | Some '\\' ->
                            appendEscape '\\'
                            loop ()
                        | Some 'u' ->
                            match parseUnicodeEscape st startPos with
                            | Ok value ->
                                sb.Append(value) |> ignore
                                loop ()
                            | Error msg ->
                                error <- Some msg
                        | Some other ->
                            error <- Some $"Unknown escape '\\{other}' at {startPos}"
                        | None -> error <- Some $"Unterminated escape starting at {startPos}"
                | Some '\n'
                | Some '\r' ->
                    error <- Some $"Unescaped newline inside string starting at {startPos}"
                | Some c ->
                    appendEscape c
                    loop ()

        loop ()

        match error with
        | Some msg -> Error msg
        | None -> Ok(sb.ToString())

    let private hasClosingHashes (st: LexerState) (hashCount: int) =
        let mutable idx = 0
        let mutable ok = true

        while ok && idx < hashCount do
            match peekAhead st idx with
            | Some '#' -> idx <- idx + 1
            | _ -> ok <- false

        ok

    let private lexRawQuoted (st: LexerState) (startPos: int) (hashCount: int) =
        let sb = StringBuilder()
        let mutable finished = false
        let mutable error: string option = None

        let rec loop () =
            if not finished && error.IsNone then
                match advance st with
                | None -> error <- Some $"Unterminated raw string starting at {startPos}"
                | Some '"' ->
                    if hasClosingHashes st hashCount then
                        advanceMany st hashCount
                        finished <- true
                    else
                        sb.Append('"') |> ignore
                        loop ()
                | Some '\r' ->
                    if peek st = Some '\n' then
                        advance st |> ignore

                    bumpLine st
                    error <- Some $"Raw strings cannot span multiple lines (started at {startPos})"
                | Some '\n' ->
                    bumpLine st
                    error <- Some $"Raw strings cannot span multiple lines (started at {startPos})"
                | Some c ->
                    sb.Append(c) |> ignore
                    loop ()

        loop ()

        match error with
        | Some msg -> Error msg
        | None -> Ok(sb.ToString())

    let private dedentContent (text: string) (indent: string) (startPos: int) =
        if text.Length = 0 || String.IsNullOrEmpty indent then
            Ok text
        else
            let lines =
                text.Split([| '\n' |], StringSplitOptions.None)

            let sb = StringBuilder()
            let mutable idx = 0
            let mutable error: string option = None

            while idx < lines.Length && error.IsNone do
                if idx > 0 then
                    sb.Append('\n') |> ignore

                let line = lines[idx]

                if String.IsNullOrWhiteSpace line then
                    ()
                elif line.StartsWith(indent) then
                    sb.Append(line.Substring(indent.Length)) |> ignore
                else
                    error <- Some $"Line {idx + 1} of multi-line string does not match indentation declared at {startPos}"

                idx <- idx + 1

            match error with
            | Some msg -> Error msg
            | None -> Ok(sb.ToString())

    let private lexMultiline (st: LexerState) (startPos: int) (isRaw: bool) (hashCount: int) =
        let closingSuffix =
            if isRaw then new String('#', hashCount) else String.Empty

        let closing = "\"\"\"" + closingSuffix
        let content = StringBuilder()
        let currentLine = StringBuilder()
        let mutable result = ""
        let mutable error: string option = None
        let mutable finished = false

        let requireInitialNewline () =
            match peek st with
            | Some '\n' ->
                advance st |> ignore
                bumpLine st
                Ok ()
            | Some '\r' ->
                advance st |> ignore

                match peek st with
                | Some '\n' -> advance st |> ignore
                | _ -> ()

                bumpLine st
                Ok ()
            | _ -> Error $"Multi-line string must start with a newline at {startPos}"

        let inline currentLineIsWhitespace () =
            currentLine.ToString() |> Seq.forall isWhitespaceChar

        let appendChar (c: char) =
            content.Append(c) |> ignore
            currentLine.Append(c) |> ignore

        let appendString (text: string) =
            content.Append(text) |> ignore
            currentLine.Append(text) |> ignore

        let rec loop () =
            if error.IsNone && not finished then
                if isAtSequence st closing then
                    if currentLineIsWhitespace () then
                        let indent = currentLine.ToString()

                        if indent.Length > 0 && content.Length < indent.Length then
                            error <- Some $"Invalid indentation before closing delimiter at {startPos}"
                        else
                            if indent.Length > 0 then
                                content.Remove(content.Length - indent.Length, indent.Length) |> ignore

                            if content.Length > 0 && content[content.Length - 1] = '\n' then
                                content.Remove(content.Length - 1, 1) |> ignore

                            match dedentContent (content.ToString()) indent startPos with
                            | Ok dedented ->
                                result <- dedented
                                advanceMany st closing.Length
                                finished <- true
                            | Error msg -> error <- Some msg
                    else
                        error <- Some $"Closing delimiter must be on its own line at {startPos}"
                else
                    match advance st with
                    | None -> error <- Some $"Unterminated multi-line string starting at {startPos}"
                    | Some '\\' when not isRaw ->
                        match peek st with
                        | Some c when isWhitespaceChar c ->
                            consumeEscapedWhitespace st
                            loop ()
                        | _ ->
                            match advance st with
                            | Some 'n' ->
                                appendString "\n"
                                loop ()
                            | Some 'r' ->
                                appendString "\r"
                                loop ()
                            | Some 't' ->
                                appendChar '\t'
                                loop ()
                            | Some 'b' ->
                                appendChar '\b'
                                loop ()
                            | Some 'f' ->
                                appendChar '\u000C'
                                loop ()
                            | Some 's' ->
                                appendChar ' '
                                loop ()
                            | Some '"' ->
                                appendChar '"'
                                loop ()
                            | Some '\\' ->
                                appendChar '\\'
                                loop ()
                            | Some 'u' ->
                                match parseUnicodeEscape st startPos with
                                | Ok value ->
                                    appendString value
                                    loop ()
                                | Error msg -> error <- Some msg
                            | Some other ->
                                error <- Some $"Unknown escape '\\{other}' in multi-line string at {startPos}"
                            | None -> error <- Some $"Unterminated escape in multi-line string at {startPos}"
                    | Some '\r' ->
                        if peek st = Some '\n' then
                            advance st |> ignore

                        content.Append('\n') |> ignore
                        currentLine.Clear() |> ignore
                        bumpLine st
                        loop ()
                    | Some '\n' ->
                        content.Append('\n') |> ignore
                        currentLine.Clear() |> ignore
                        bumpLine st
                        loop ()
                    | Some c ->
                        appendChar c
                        loop ()

        match requireInitialNewline () with
        | Error msg -> Error msg
        | Ok _ ->
            loop ()

            match error with
            | Some msg -> Error msg
            | None -> Ok result

    /// Recursive identifier lexer: [A-Za-z_][A-Za-z0-9_-]*
    let rec private lexIdent (st: LexerState) (sb: StringBuilder) =
        match peek st with
        | Some c when isIdentChar c ->
            advance st |> ignore
            sb.Append(c) |> ignore
            lexIdent st sb
        | _ -> sb.ToString()

    let private lexBaseNumber (st: LexerState) (startPos: int) (sb: StringBuilder) (digitPredicate: char -> bool) (kindName: string) =
        let mutable hasDigits = false
        let mutable lastWasUnderscore = false
        let mutable error: string option = None

        let rec loop () =
            if error.IsNone then
                match peek st with
                | Some c when digitPredicate c ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    hasDigits <- true
                    lastWasUnderscore <- false
                    loop ()
                | Some '_' ->
                    if not hasDigits || lastWasUnderscore then
                        error <- Some $"Invalid underscore in {kindName} literal at {startPos}"
                    else
                        match peekAhead st 1 with
                        | Some next when digitPredicate next ->
                            advance st |> ignore
                            sb.Append('_') |> ignore
                            lastWasUnderscore <- true
                            loop ()
                        | _ ->
                            error <- Some $"Invalid underscore in {kindName} literal at {startPos}"
                            advance st |> ignore
                            sb.Append('_') |> ignore
                            lastWasUnderscore <- true
                            loop ()
                | _ -> ()

        loop ()

        match error with
        | Some msg -> Error msg
        | None when not hasDigits -> Error $"Invalid {kindName} literal at {startPos}"
        | None when lastWasUnderscore -> Error $"Invalid {kindName} literal at {startPos}"
        | None -> Ok(sb.ToString())

    let private lexDecimalNumber (st: LexerState) (startPos: int) (sb: StringBuilder) (hasInitialDigits: bool) =
        let mutable seenDigits = hasInitialDigits
        let mutable seenFraction = false
        let mutable seenFractionDigits = false
        let mutable seenExponent = false
        let mutable seenExponentDigits = false
        let mutable expectExponentSign = false
        let mutable lastWasUnderscore = false
        let mutable prevWasDigit = hasInitialDigits
        let mutable error: string option = None

        let rec loop () =
            if error.IsNone then
                match peek st with
                | Some c when Char.IsDigit c ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    lastWasUnderscore <- false
                    prevWasDigit <- true

                    if seenExponent then
                        seenExponentDigits <- true
                        expectExponentSign <- false
                    elif seenFraction then
                        seenFractionDigits <- true
                    else
                        seenDigits <- true

                    loop ()
                | Some '_' ->
                    if lastWasUnderscore || not prevWasDigit then
                        error <- Some $"Underscore must separate digits in number starting at {startPos}"
                    else
                        match peekAhead st 1 with
                        | Some next when Char.IsDigit next ->
                            advance st |> ignore
                            sb.Append('_') |> ignore
                            lastWasUnderscore <- true
                            prevWasDigit <- false
                            loop ()
                        | _ ->
                            error <- Some $"Underscore must be followed by digit in number starting at {startPos}"
                            advance st |> ignore
                            sb.Append('_') |> ignore
                            lastWasUnderscore <- true
                            prevWasDigit <- false
                            loop ()
                | Some '.' when not seenFraction && not seenExponent ->
                    if not seenDigits then
                        error <- Some $"Leading decimal point without digits at {startPos}"
                    else
                        advance st |> ignore
                        sb.Append('.') |> ignore
                        seenFraction <- true
                        prevWasDigit <- false
                        lastWasUnderscore <- false
                        loop ()
                | Some ('e' | 'E' as c) when not seenExponent ->
                    if not seenDigits && not seenFractionDigits then
                        error <- Some $"Exponent requires digits before it at {startPos}"
                    else
                        advance st |> ignore
                        sb.Append(c) |> ignore
                        seenExponent <- true
                        expectExponentSign <- true
                        prevWasDigit <- false
                        lastWasUnderscore <- false
                        loop ()
                | Some ('+' | '-' as c) when expectExponentSign ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    expectExponentSign <- false
                    prevWasDigit <- false
                    loop ()
                | _ -> ()

        loop ()

        match error with
        | Some msg -> Error msg
        | None when lastWasUnderscore -> Error $"Trailing underscore in number starting at {startPos}"
        | None when expectExponentSign -> Error $"Exponent missing digits in number starting at {startPos}"
        | None when seenFraction && not seenFractionDigits -> Error $"Fractional part missing digits in number starting at {startPos}"
        | None when seenExponent && not seenExponentDigits -> Error $"Exponent missing digits in number starting at {startPos}"
        | None when not seenDigits && not seenFractionDigits -> Error $"Number must contain digits at {startPos}"
        | None -> Ok(sb.ToString())

    let private lexNumberLiteral (st: LexerState) (firstChar: char) =
        let startPos = currentPos st - 1
        let sb = StringBuilder()
        sb.Append(firstChar) |> ignore

        let emit result =
            match result with
            | Ok literal -> Number literal
            | Error msg -> TokenError(msg, startPos)

        let emitDecimal hasInitialDigits =
            emit (lexDecimalNumber st startPos sb hasInitialDigits)

        let parsePrefixed digitPredicate kindName =
            emit (lexBaseNumber st startPos sb digitPredicate kindName)

        match firstChar with
        | '-' ->
            match peek st with
            | Some '0' ->
                advance st |> ignore
                sb.Append('0') |> ignore

                match peek st with
                | Some ('x' | 'X' as c) ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    parsePrefixed isHexDigit "hexadecimal"
                | Some ('o' | 'O' as c) ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    parsePrefixed isOctDigit "octal"
                | Some ('b' | 'B' as c) ->
                    advance st |> ignore
                    sb.Append(c) |> ignore
                    parsePrefixed isBinDigit "binary"
                | _ -> emitDecimal true
            | _ -> emitDecimal false
        | '0' ->
            match peek st with
            | Some ('x' | 'X' as c) ->
                advance st |> ignore
                sb.Append(c) |> ignore
                parsePrefixed isHexDigit "hexadecimal"
            | Some ('o' | 'O' as c) ->
                advance st |> ignore
                sb.Append(c) |> ignore
                parsePrefixed isOctDigit "octal"
            | Some ('b' | 'B' as c) ->
                advance st |> ignore
                sb.Append(c) |> ignore
                parsePrefixed isBinDigit "binary"
            | _ -> emitDecimal true
        | _ -> emitDecimal true

    /// Skip a single-line comment until (but not including) the newline.
    /// The newline will be processed by the main tokenization loop.
    let rec private skipLineComment (st: LexerState) =
        match peek st with
        | None -> ()
        | Some '\n'
        | Some '\r' -> () // Stop at newline, don't consume it
        | Some _ ->
            advance st |> ignore
            skipLineComment st

    /// Skip a /* ... */ comment, supporting nesting.
    let private skipBlockComment (st: LexerState) =
        let mutable depth = 1

        let rec loop () =
            if depth > 0 then
                match advance st with
                | None -> depth <- 0
                | Some '/' ->
                    match peek st with
                    | Some '*' ->
                        advance st |> ignore
                        depth <- depth + 1
                        loop ()
                    | _ -> loop ()
                | Some '*' ->
                    match peek st with
                    | Some '/' ->
                        advance st |> ignore
                        depth <- depth - 1
                        loop ()
                    | _ -> loop ()
                | Some '\r' ->
                    if peek st = Some '\n' then
                        advance st |> ignore

                    bumpLine st
                    loop ()
                | Some '\n' ->
                    bumpLine st
                    loop ()
                | Some _ -> loop ()

        loop ()

    let private skipBom (st: LexerState) =
        if st.Pos = 0 then
            match peek st with
            | Some '\uFEFF' ->
                advance st |> ignore
                st.Col <- 1
            | _ -> ()

    /// Read one token from the current position.
    let rec private nextToken (st: LexerState) : Token =
        match advance st with
        | None -> Eof
        | Some c ->
            match c with
            | ' '
            | '\t' -> nextToken st
            | '\n'
            | '\r' ->
                if c = '\r' && peek st = Some '\n' then
                    advance st |> ignore

                bumpLine st

                match peek st with
                | Some '/' -> nextToken st
                | _ -> Newline
            | '/' ->
                match peek st with
                | Some '-' ->
                    advance st |> ignore
                    SlashDash
                | Some '/' ->
                    advance st |> ignore
                    skipLineComment st
                    nextToken st
                | Some '*' ->
                    advance st |> ignore
                    skipBlockComment st
                    nextToken st
                | _ -> Ident "/"
            | '{' -> LBrace
            | '}' -> RBrace
            | '(' -> TypeLParen
            | ')' -> TypeRParen
            | ';' -> Semicolon
            | '=' -> Eq
            | '"' ->
                let start = currentPos st - 1

                if peek st = Some '"' && peekAhead st 1 = Some '"' then
                    advanceMany st 2

                    match lexMultiline st start false 0 with
                    | Ok value -> String value
                    | Error msg -> TokenError(msg, start)
                else
                    match lexQuoted st start with
                    | Ok value -> String value
                    | Error msg -> TokenError(msg, start)
            | c when isDigit c -> lexNumberLiteral st c
            | '-' when (peek st |> Option.map isDigit = Some true) -> lexNumberLiteral st '-'
            | '#' ->
                let start = currentPos st - 1
                let rec countExtraHashes offset =
                    match peekAhead st offset with
                    | Some '#' -> countExtraHashes (offset + 1)
                    | _ -> offset

                let extraHashes = countExtraHashes 0
                let hashCount = 1 + extraHashes
                let charAfterHashes = peekAhead st extraHashes
                let isRawMultiline =
                    match charAfterHashes, peekAhead st (extraHashes + 1), peekAhead st (extraHashes + 2) with
                    | Some '"', Some '"', Some '"' -> true
                    | _ -> false

                let isRawQuoted = not isRawMultiline && charAfterHashes = Some '"'

                if isRawMultiline then
                    advanceMany st extraHashes
                    advanceMany st 3

                    match lexMultiline st start true hashCount with
                    | Ok value -> RawString value
                    | Error msg -> TokenError(msg, start)
                elif isRawQuoted then
                    advanceMany st extraHashes
                    advance st |> ignore

                    match lexRawQuoted st start hashCount with
                    | Ok value -> RawString value
                    | Error msg -> TokenError(msg, start)
                else
                    let sb = StringBuilder()
                    sb.Append('#') |> ignore

                    match peek st with
                    | Some '-' ->
                        advance st |> ignore
                        sb.Append('-') |> ignore
                    | _ -> ()

                    let ident = lexIdent st sb

                    match ident with
                    | "#true"
                    | "#false"
                    | "#null"
                    | "#inf"
                    | "#-inf"
                    | "#nan" -> Keyword ident
                    | _ -> TokenError($"Unknown keyword {ident}", start)
            | c when isAlpha c ->
                let sb = StringBuilder()
                sb.Append(c) |> ignore
                let ident = lexIdent st sb

                match ident with
                | _ -> Ident ident
            | '\uFEFF' -> TokenError("Unexpected BOM in document", currentPos st - 1)
            | other -> TokenError($"Unexpected character '%c{other}'", currentPos st)

    let rec private lexAll (st: LexerState) (acc: Token list) =
        let tok = nextToken st
        let newAcc = tok :: acc

        match tok with
        | Eof -> List.rev newAcc
        | _ -> lexAll st newAcc


    /// Tokenize a source string into a list of tokens.
    let tokenize (source: string) : Token list =
        let st =
            { Source = source
              Pos = 0
              Line = 1
              Col = 1 }

        skipBom st

        let rec lexAll acc =
            let tok = nextToken st
            let acc' = tok :: acc

            match tok with
            | Eof -> List.rev acc'
            | _ -> lexAll acc'


        lexAll []
        |> List.skipWhile (function
            | Newline -> true
            | _ -> false)
        |> List.rev
        |> List.skipWhile (function
            | Newline -> true
            | _ -> false)
        |> List.rev
        |> fun (tokens) ->
            match List.rev tokens with
            | Eof :: Newline :: rest -> (Eof :: rest) |> List.rev
            | _ -> tokens
