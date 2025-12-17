namespace KDLFSharp.Core

/// Internal token stream wrapper for lookahead & consumption.
type private TokenStream =
    { Tokens: Token array
      mutable Index: int }

module private Stream =
    let current ts =
        if ts.Index >= ts.Tokens.Length then
            Eof
        else
            ts.Tokens[ts.Index]

    let advance ts =
        if ts.Index < ts.Tokens.Length then
            ts.Index <- ts.Index + 1

    let eat ts tok =
        if current ts = tok then
            advance ts
            true
        else
            false

/// Parser result type
type ParseResult<'a> = Result<'a, AstError list>

module Parser =
    let rec private skipTrivialTokens (ts: TokenStream) =
        match Stream.current ts with
        | Newline
        | Semicolon ->
            Stream.advance ts
            skipTrivialTokens ts
        | _ -> ()

    let rec private parseDocument (ts: TokenStream) : Node list * AstError list = parseNodes ts []

    and private parseNodes (ts: TokenStream) (acc: Node list) : Node list * AstError list =
        match Stream.current ts with
        | Eof
        | RBrace -> (List.rev acc, [])
        | Newline ->
            Stream.advance ts
            parseNodes ts acc
        | Semicolon ->
            Stream.advance ts
            parseNodes ts acc
        | SlashDash ->
            Stream.advance ts
            skipTrivialTokens ts
            let _, skipErrs = parseOneNode ts
            let nodes, restErrs = parseNodes ts acc
            (nodes, skipErrs @ restErrs)
        | _ ->
            let nodeRes, errs = parseOneNode ts

            match nodeRes with
            | Some n ->
                let nodes, restErrs = parseNodes ts (n :: acc)
                (nodes, errs @ restErrs)
            | None -> (List.rev acc, errs)

    and private parseOneNode (ts: TokenStream) : Node option * AstError list =
        let mutable errors: AstError list = []
        let typeAnn, typeErrs = parseOptTypeAnn ts
        errors <- errors @ typeErrs

        match Stream.current ts with
        | Ident name ->
            Stream.advance ts
            let args, props, entryErrs = parseEntries ts [] [] false
            errors <- errors @ entryErrs
            let children, childErrs = parseOptChildren ts
            errors <- errors @ childErrs
            let _ = parseTerminator ts

            let node =
                { TypeAnn = typeAnn
                  Name = name
                  Arguments = args
                  Properties = props
                  Children = children
                  Span = None
                  OriginalText = None }

            Some node, errors
        | tok ->
            let err = AstError.unexpectedValue $"Expected node name, found %A{tok}"
            Stream.advance ts
            None, (err :: errors)

    and private parseOptTypeAnn (ts: TokenStream) =
        match Stream.current ts with
        | TypeLParen ->
            Stream.advance ts

            match Stream.current ts with
            | Ident t ->
                Stream.advance ts

                match Stream.current ts with
                | TypeRParen ->
                    Stream.advance ts
                    (Some t, [])
                | other ->
                    Stream.advance ts
                    (Some t, [ AstError.invalidTypeAnnotation $"Expected ')', found %A{other}" ])
            | other ->
                Stream.advance ts

                (None, [ AstError.invalidTypeAnnotation $"Expected type name, found %A{other}" ])
        | _ -> (None, [])

    /// entries := (prop | arg | slashdash)* until { or newline or ; or } or eof
    and private parseEntries (ts: TokenStream) (argsAcc: Value list) (propsAcc: Property list) (childrenOnly: bool) =
        match Stream.current ts with
        | Newline
        | Semicolon
        | Eof
        | RBrace
        | LBrace -> (List.rev argsAcc, List.rev propsAcc, [])
        | SlashDash ->
            Stream.advance ts
            skipTrivialTokens ts
            let skippedChildren, errs = skipOneEntry ts
            let nextChildrenOnly = childrenOnly || skippedChildren

            let args, props, restErrs = parseEntries ts argsAcc propsAcc nextChildrenOnly
            let combinedErrs = errs @ restErrs

            if childrenOnly && not skippedChildren then
                (args, props, AstError.unexpectedValue "Only children blocks may follow a slashdashed children block." :: combinedErrs)
            else
                (args, props, combinedErrs)
        | _ when childrenOnly ->
            (List.rev argsAcc, List.rev propsAcc, [ AstError.unexpectedValue "Only children blocks may follow a slashdashed children block." ])
        | _ ->
            match Stream.current ts with
            | Ident _key ->
                match lookaheadEq ts with
                | true ->
                    let prop, errs = parseProperty ts
                    let args, props, errs2 = parseEntries ts argsAcc (prop :: propsAcc) childrenOnly
                    (args, props, errs @ errs2)
                | false ->
                    let value, errs = parseValue ts
                    let args, props, errs2 = parseEntries ts (value :: argsAcc) propsAcc childrenOnly
                    (args, props, errs @ errs2)
            | _ ->
                let value, errs = parseValue ts
                let args, props, errs2 = parseEntries ts (value :: argsAcc) propsAcc childrenOnly
                (args, props, errs @ errs2)

    and private lookaheadEq (ts: TokenStream) =
        if ts.Index + 1 < ts.Tokens.Length then
            match ts.Tokens[ts.Index + 1] with
            | Eq -> true
            | _ -> false
        else
            false

    and private parseProperty (ts: TokenStream) =
        match Stream.current ts with
        | Ident key ->
            Stream.advance ts

            match Stream.current ts with
            | Eq ->
                Stream.advance ts
                let v, errs = parseValue ts
                ({ Key = key; Value = v }, errs)
            | other ->
                Stream.advance ts

                ({ Key = key; Value = Value.Null }, [ AstError.unexpectedValue $"Expected '=', found %A{other}" ])
        | tok ->
            Stream.advance ts

            ({ Key = "_"; Value = Value.Null }, [ AstError.unexpectedValue $"Expected property key, found %A{tok}" ])

    and private parseValue (ts: TokenStream) =
        match Stream.current ts with
        | String s ->
            Stream.advance ts
            (Value.String(s, None), [])
        | RawString s ->
            Stream.advance ts
            (Value.String(s, None), [])
        | Number n ->
            Stream.advance ts
            (Value.Number(NumberLiteral.ofRaw n, None), [])
        | Keyword "#true" ->
            Stream.advance ts
            (Value.Boolean true, [])
        | Keyword "#false" ->
            Stream.advance ts
            (Value.Boolean false, [])
        | Keyword "#null" ->
            Stream.advance ts
            (Value.Null, [])
        | Keyword "#inf" ->
            Stream.advance ts
            (Value.Number(NumberLiteral.special "#inf" SpecialNumberKind.Infinity, None), [])
        | Keyword "#-inf" ->
            Stream.advance ts
            (Value.Number(NumberLiteral.special "#-inf" SpecialNumberKind.NegativeInfinity, None), [])
        | Keyword "#nan" ->
            Stream.advance ts
            (Value.Number(NumberLiteral.special "#nan" SpecialNumberKind.NotANumber, None), [])
        | tok ->
            Stream.advance ts
            (Value.Null, [ AstError.unexpectedValue $"Unexpected value token %A{tok}" ])

    and private parseOptChildren (ts: TokenStream) =
        match Stream.current ts with
        | LBrace ->
            Stream.advance ts
            let nodes, errs = parseNodes ts []

            match Stream.current ts with
            | RBrace ->
                Stream.advance ts
                (nodes, errs)
            | other ->
                Stream.advance ts
                (nodes, AstError.unexpectedValue $"Expected '}}', found %A{other}" :: errs)
        | _ -> ([], [])

    and private parseTerminator (ts: TokenStream) =
        match Stream.current ts with
        | Newline
        | Semicolon -> Stream.advance ts
        | _ -> ()

    /// Skip exactly one entry after a slashdash. Returns true if a children block was skipped.
    and private skipOneEntry (ts: TokenStream) =
        skipTrivialTokens ts

        match Stream.current ts with
        | LBrace ->
            Stream.advance ts

            let rec skipChildren depth =
                match Stream.current ts with
                | LBrace ->
                    Stream.advance ts
                    skipChildren (depth + 1)
                | RBrace ->
                    Stream.advance ts

                    if depth > 0 then
                        skipChildren (depth - 1)
                | Eof -> ()
                | _ ->
                    Stream.advance ts
                    skipChildren depth

            skipChildren 0
            (true, [])
        | TypeLParen ->
            let _, typeErrs = parseOptTypeAnn ts
            let skippedChildren, restErrs = skipOneEntry ts
            (skippedChildren, typeErrs @ restErrs)
        | Ident _ when lookaheadEq ts ->
            Stream.advance ts
            Stream.advance ts
            let _, valueErrs = parseValue ts
            (false, valueErrs)
        | _ ->
            let _, valueErrs = parseValue ts
            (false, valueErrs)

    /// Parse a KDL source string into a Document.
    let parse (source: string) : ParseResult<Document> =
        let tokens = Lexer.tokenize source |> Array.ofList
        let ts = { Tokens = tokens; Index = 0 }
        let nodes, errs = parseDocument ts
        if errs.IsEmpty then Ok nodes else Error errs
