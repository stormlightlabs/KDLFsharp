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

    let peek (st: LexerState) =
        if st.Pos >= st.Source.Length then
            None
        else
            Some st.Source[st.Pos]

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

module Lexer =
    /// Recursive string lexer for "..."
    ///
    /// TODO: extend to multiline (""" ... """) and raw (#"..."#) forms.
    let rec private lexQuoted (st: LexerState) (sb: StringBuilder) =
        match advance st with
        | None -> sb.ToString()
        | Some '"' -> sb.ToString()
        | Some '\\' ->
            match advance st with
            | Some 'n' ->
                sb.Append('\n') |> ignore
                lexQuoted st sb
            | Some 't' ->
                sb.Append('\t') |> ignore
                lexQuoted st sb
            | Some 'r' ->
                sb.Append('\r') |> ignore
                lexQuoted st sb
            | Some '"' ->
                sb.Append('"') |> ignore
                lexQuoted st sb
            | Some '\\' ->
                sb.Append('\\') |> ignore
                lexQuoted st sb
            | Some c ->
                sb.Append(c) |> ignore
                lexQuoted st sb
            | None -> sb.ToString()
        | Some c ->
            sb.Append(c) |> ignore
            lexQuoted st sb

    /// Recursive identifier lexer: [A-Za-z_][A-Za-z0-9_-]*
    let rec private lexIdent (st: LexerState) (sb: StringBuilder) =
        match peek st with
        | Some c when isIdentChar c ->
            advance st |> ignore
            sb.Append(c) |> ignore
            lexIdent st sb
        | _ -> sb.ToString()

    /// Recursive number lexer; we keep the literal as-is for the parser.
    ///
    /// TODO: tighten the grammar to match KDL 2.0 numeric rules exactly.
    let rec private lexNumber (st: LexerState) (sb: StringBuilder) =
        match peek st with
        | Some c when isDigit c || c = '_' || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-' ->
            advance st |> ignore
            sb.Append(c) |> ignore
            lexNumber st sb
        | _ -> sb.ToString()

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

    /// Skip a /* ... */ comment.
    ///
    /// TODO: support nested comments if the spec requires it.
    let rec private skipBlockComment (st: LexerState) =
        match advance st with
        | None -> ()
        | Some '*' ->
            match peek st with
            | Some '/' ->
                advance st |> ignore
                ()
            | _ -> skipBlockComment st
        | Some '\n' ->
            bumpLine st
            skipBlockComment st
        | Some _ -> skipBlockComment st

    /// TODO: handle BOM at start of file (U+FEFF).
    let private skipBom (st: LexerState) =
        match peek st with
        | Some '\uFEFF' -> advance st |> ignore
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
                let start = currentPos st
                let s = lexQuoted st (StringBuilder())

                match peek st with
                | None -> TokenError($"Unterminated string starting at {start}", start)
                | _ -> String s
            | c when isDigit c ->
                let sb = StringBuilder()
                sb.Append(c) |> ignore
                let lit = lexNumber st sb
                Number lit
            | '-' when (peek st |> Option.map isDigit = Some true) ->
                let sb = StringBuilder()
                sb.Append('-') |> ignore
                let lit = lexNumber st sb
                Number lit
            | '#' ->
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
                | _ -> TokenError($"Unknown keyword {ident}", currentPos st)
            | c when isAlpha c ->
                let sb = StringBuilder()
                sb.Append(c) |> ignore
                let ident = lexIdent st sb

                match ident with
                | "#true"
                | "#false"
                | "#null"
                | "#inf"
                | "#-inf"
                | "#nan" -> Keyword ident
                | _ -> Ident ident
            // TODO: handle raw strings (#"..."#) and multiline ("""...""")
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
