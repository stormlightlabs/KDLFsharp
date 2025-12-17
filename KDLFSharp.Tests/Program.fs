open Expecto
open KDLFSharp.Core

/// Helper to stringify token list (for snapshot comparison)
let private showTokens toks =
    toks |> List.map string |> String.concat " "

/// Run lexer on input and return stringified tokens
let private lex input = Lexer.tokenize input |> showTokens

let private parseSingleNode input =
    match Parser.parse input with
    | Ok [ node ] -> node
    | Ok nodes -> failtestf "Expected exactly 1 node, found %d" nodes.Length
    | Error errs -> failtestf "Parse failed: %A" errs

let private parseNodes input =
    match Parser.parse input with
    | Ok nodes -> nodes
    | Error errs -> failtestf "Parse failed: %A" errs

let private expectParseError input =
    match Parser.parse input with
    | Error errs -> Expect.isFalse errs.IsEmpty "Expected parse errors"
    | Ok nodes -> failtestf "Expected parse error but got AST: %A" nodes

let private propMap (node: Node) =
    node.Properties |> List.map (fun p -> p.Key, p.Value) |> Map.ofList

let private stringOfValue value =
    match value with
    | Value.String(s, _) -> s
    | other -> failtestf "Expected string value, got %A" other

let private numberLiteral value =
    match value with
    | Value.Number(lit, _) -> lit
    | other -> failtestf "Expected number value, got %A" other

[<Tests>]
let tests =
    testList
        "Lexer tests"
        [ test "identifiers and punctuation" {
              let input = """foo bar;baz"""
              let toks = lex input
              Expect.equal toks "Ident(foo) Ident(bar) ; Ident(baz) <eof>" "basic identifiers"
          }
          test "quoted string" {
              let input = """ "hello" """
              let toks = lex input
              Expect.equal toks "String(hello) <eof>" "quoted string literal"
          }
          test "number literal" {
              let input = "42"
              let toks = lex input
              Expect.equal toks "Number(42) <eof>" "numeric literal"
          }
          test "boolean and null keywords" {
              let input = "#true #false #null"

              Expect.equal (lex input) "Keyword(#true) Keyword(#false) Keyword(#null) <eof>" "keyword tokens"
          }

          test "brace tokens" {
              let input = "{ }"
              Expect.equal (lex input) "{ } <eof>" "braces recognized"
          }

          test "comments are skipped" {
              let input =
                  """
        // comment
        foo /* block comment */ bar
        """

              let toks = lex input
              Expect.equal toks "Ident(foo) Ident(bar) <eof>" "comments stripped"
          }

          test "slashdash emitted" {
              let input = "/- foo"
              let toks = lex input
              Expect.equal toks "/- Ident(foo) <eof>" "slashdash tokenized"
          }

          test "newlines normalized" {
              let input = "foo\r\nbar\nbaz"
              let toks = lex input

              Expect.equal toks "Ident(foo) ⏎ Ident(bar) ⏎ Ident(baz) <eof>" "newlines normalized"
          }

          test "property syntax" {
              let input = "key=value"
              let toks = lex input
              Expect.equal toks "Ident(key) = Ident(value) <eof>" "property tokens"
          }

          test "unterminated string yields error" {
              let input = "\"foo"
              let toks = Lexer.tokenize input

              let hasError =
                  toks
                  |> List.exists (function
                      | TokenError _ -> true
                      | _ -> false)

              Expect.isTrue hasError "unterminated string should produce error"
          }

          test "multiline string literal tokenized" {
              let input = "msg \"\"\"\nline1\nline2\n\"\"\""
              let toks = lex input
              Expect.equal toks "Ident(msg) String(line1\nline2) <eof>" "multiline string collapsed"
          }

          test "raw string literal tokenized" {
              let input = "msg #\"\\n will be literal\"#"
              let toks = lex input
              Expect.equal toks "Ident(msg) RawString(\\n will be literal) <eof>" "raw string retains escapes"
          } ]

[<Tests>]
let documentStructureTests =
    testList
        "A. Document structure / node termination"
        [ test "A01: Empty document" {
              let input = ""
              let result = Parser.parse input

              match result with
              | Ok nodes -> Expect.isEmpty nodes "empty document should produce empty AST"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "A02: Two top-level nodes separated by newline" {
              let input = "zilker\npease"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 2 "should have 2 nodes"
                  Expect.equal nodes[0].Name "zilker" "first node name"
                  Expect.equal nodes[1].Name "pease" "second node name"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "A03: Node terminated by semicolon" {
              let input = "zilker; pease;"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 2 "should have 2 nodes"
                  Expect.equal nodes[0].Name "zilker" "first node"
                  Expect.equal nodes[1].Name "pease" "second node"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "A04: Node terminated by EOF (no trailing newline)" {
              let input = "zilker"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 1 "should have 1 node"
                  Expect.equal nodes[0].Name "zilker" "node name"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "A05: Multiple nodes in single-line children block (must use semicolons)" {
              let input = "parks { zilker; pease; auditorium_shores; }"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 1 "should have 1 parent node"
                  let parks = nodes[0]
                  Expect.equal parks.Name "parks" "parent node name"
                  Expect.hasLength parks.Children 3 "should have 3 children"
                  Expect.equal parks.Children[0].Name "zilker" "first child"
                  Expect.equal parks.Children[1].Name "pease" "second child"
                  Expect.equal parks.Children[2].Name "auditorium_shores" "third child"
              | Error errs -> failtestf "Parse failed: %A" errs
          } ]

[<Tests>]
let whitespaceTests =
    testList
        "B. Whitespace, comments, slashdash"
        [ test "B01: Single-line comment is whitespace" {
              let input = "zilker // famous skyline view\npease"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 2 "should have 2 nodes"
                  Expect.equal nodes[0].Name "zilker" "first node"
                  Expect.equal nodes[1].Name "pease" "second node"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "B02: Multi-line comment is whitespace (and may be nested)" {
              let input = "/* outer /* inner */ still outer */ zilker"
              let nodes = parseNodes input
              Expect.hasLength nodes 1 "nested comments treated as whitespace"
              Expect.equal nodes[0].Name "zilker" "node should survive after comments"
          }

          test "B03: Slashdash entire node (node is removed)" {
              let input = "/- zilker\npease"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 1 "should have 1 node (zilker removed)"
                  Expect.equal nodes[0].Name "pease" "remaining node"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          test "B04: Slashdash argument removes that argument only" {
              let input = "trail \"Lady Bird Lake\" /- 3.1 open=#true"
              let node = parseSingleNode input
              Expect.equal node.Arguments.Length 1 "only one argument remains"

              match node.Arguments[0] with
              | Value.String(s, _) -> Expect.equal s "Lady Bird Lake" "string argument preserved"
              | v -> failtestf "Expected string argument, got %A" v

              let props = propMap node

              match Map.tryFind "open" props with
              | Some(Value.Boolean true) -> ()
              | other -> failtestf "Expected open property, got %A" other
          }

          test "B05: Slashdash property removes key+value" {
              let input = "park name=\"Zilker\" /- hidden=#true open=#true"
              let node = parseSingleNode input
              let props = propMap node
              Expect.equal props.Count 2 "slashdashed property removed"

              match Map.tryFind "name" props with
              | Some(Value.String(s, _)) -> Expect.equal s "Zilker" "name preserved"
              | other -> failtestf "Expected name property, got %A" other

              match Map.tryFind "open" props with
              | Some(Value.Boolean true) -> ()
              | other -> failtestf "Expected open=#true, got %A" other
          }

          test "B06: Slashdash 'just the property value' is illegal" {
              let input = "park hidden=/- #true"
              expectParseError input
          }

          test "B07: Slashdash children block removes the whole block" {
              let input = "park \"Zilker\" /- { note \"should vanish\" }"
              let node = parseSingleNode input
              Expect.isEmpty node.Children "slashdashed children removed"
          }

          test "B08: After a slashdashed children block, only children blocks may follow" {
              let input = "park \"Zilker\" /- { a } note \"nope\""
              expectParseError input
          } ]

[<Tests>]
let lineContinuationTests =
    testList
        "C. Line continuation (backslash-escaped newline)"
        [ ptestCase "C01: Line continuation across newline"
          <| fun () ->
              // INPUT: park "Zilker"\\\nopen=#true
              // EXPECTED: park.args = ["Zilker"], park.props.open = #true
              // TODO: Backslash line continuation not implemented in lexer
              skiptest "Line continuation not implemented"

          ptestCase "C02: Line continuation allows comments after backslash"
          <| fun () ->
              // INPUT: park "Pease" \\ // continue entries on next line\nopen=#true
              // EXPECTED: args=["Pease"], props.open=#true
              // TODO: Backslash line continuation not implemented in lexer
              skiptest "Line continuation not implemented"

          ptestCase "C03: Line continuation may include multiline comments between \\ and newline"
          <| fun () ->
              // INPUT: park "Walnut Creek" \\ /* ok */\nopen=#true
              // EXPECTED: props.open=#true
              // TODO: Backslash line continuation not implemented in lexer
              skiptest "Line continuation not implemented" ]

[<Tests>]
let propertiesAndArgumentsTests =
    testList
        "D. Properties and argument ordering"
        [ test "D01: Arguments preserve relative order even when properties are interleaved" {
              let node = parseSingleNode "foo 1 a=10 3 b=20 5"

              let args =
                  node.Arguments
                  |> List.choose (function
                      | Value.Number(lit, _) -> Some lit.Raw
                      | _ -> None)

              Expect.equal args [ "1"; "3"; "5" ] "argument order preserved"

              let props = propMap node

              match Map.tryFind "a" props with
              | Some(Value.Number(lit, _)) -> Expect.equal lit.Raw "10" "property a"
              | other -> failtestf "Expected property a, got %A" other

              match Map.tryFind "b" props with
              | Some(Value.Number(lit, _)) -> Expect.equal lit.Raw "20" "property b"
              | other -> failtestf "Expected property b, got %A" other
          }

          ptestCase "D02: Rightmost property wins on duplicate keys"
          <| fun () ->
              // INPUT: park name="Zilker" name="Zilker Metropolitan Park"
              // EXPECTED: park.props.name == "Zilker Metropolitan Park"
              skiptest "TODO"

          ptestCase "D03: Properties should not be treated as order-sensitive"
          <| fun () ->
              // INPUT: park a=1 b=2
              // EXPECTED: props map contains a=1 and b=2
              skiptest "TODO"

          ptestCase "D04: Property key must be a String; value must be a Value"
          <| fun () ->
              // INPUT: park 123=456
              // EXPECTED: PARSE ERROR
              skiptest "TODO" ]

[<Tests>]
let typeAnnotationTests =
    testList
        "E. Type annotations"
        [ test "E01: Type annotation on node name" {
              let node = parseSingleNode "(published)date \"1970-01-01\""
              Expect.equal node.Name "date" "node name"
              Expect.equal node.TypeAnn (Some "published") "type annotation preserved"

              match node.Arguments with
              | [ Value.String(s, _) ] -> Expect.equal s "1970-01-01" "argument captured"
              | other -> failtestf "Unexpected arguments %A" other
          }

          ptestCase "E02: Type annotation on a value"
          <| fun () ->
              // INPUT: distance (f32)3.1 unit="miles"
              // EXPECTED: distance.args = [(type="f32", value=3.1)], props.unit="miles"
              skiptest "TODO"

          ptestCase "E03: Whitespace allowed inside and around type annotation"
          <| fun () ->
              // INPUT: (  published  )  date  "1970-01-01"
              // EXPECTED: same as E01
              skiptest "TODO"

          test "E04: Slashdash can appear before a type annotation it is commenting out" {
              let node = parseSingleNode "park /- (tag)\"experimental\" open=#true"
              Expect.isEmpty node.Arguments "slashdashed typed argument removed"
              let props = propMap node

              match Map.tryFind "open" props with
              | Some(Value.Boolean true) -> ()
              | other -> failtestf "Expected open=#true, got %A" other
          } ]

[<Tests>]
let stringTests =
    testList
        "F. Strings (identifier, quoted, multiline, raw) + escapes"
        [ ptestCase "F01: Identifier string node name" <| fun () -> skiptest "TODO"

          ptestCase "F02: Quoted string node name (spaces)" <| fun () -> skiptest "TODO"

          test "F03: Identifier strings cannot contain forbidden punctuation" { expectParseError "zilker#park" }

          test "F04: Quoted string with common escapes" {
              let node = parseSingleNode "note \"Line1\\nLine2\\tTabbed\""
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "Line1\nLine2\tTabbed" "escapes processed"
          }

          test "F05: Unicode escape in quoted string" {
              let node = parseSingleNode "park name=\"Zilker \\u{26F2}\""
              let props = propMap node

              match Map.tryFind "name" props with
              | Some value ->
                  let actual = stringOfValue value
                  Expect.equal actual ("Zilker " + "\u26F2") "unicode escape converted"
              | None -> failtest "Missing name property"
          }

          test "F06: Escaped whitespace inside quoted string is discarded" {
              let node = parseSingleNode "msg \"Hello \\    World\""
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "Hello World" "escaped whitespace removed"
          }

          test "F07: Multi-line string basic (dedent rule)" {
              let node = parseSingleNode "desc \"\"\"\nFirst line\n  Indented\n\"\"\""
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "First line\n  Indented" "multiline contents preserved"
          }

          test "F08: Multi-line string cannot be single-line (illegal)" { expectParseError "desc \"\"\"nope\"\"\"" }

          test "F09: Raw single-line string (no escapes processed)" {
              let node = parseSingleNode "just #\"\\n will be literal\"#"
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "\\n will be literal" "raw string preserves backslash"
          }

          test "F10: Raw string delimiter with multiple # (can contain quotes)" {
              let node = parseSingleNode "raw ##\"hello\\n\\r\\asd\"#world\"##"
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "hello\\n\\r\\asd\"#world" "raw string keeps internal quotes"
          }

          test "F11: Raw multi-line string (no escapes; dedent rules)" {
              let node =
                  parseSingleNode "rawml #\"\"\"\nHere's a \"\"\"\nmultiline string\n\"\"\"#"

              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "Here's a \"\"\"\nmultiline string" "raw multiline retains content"
          }

          ptestCase "F12: Disallowed literal code points must not appear literally"
          <| fun () ->
              // INPUT: note "bad<NUL>char"
              // EXPECTED: PARSE ERROR (U+0000..0008, U+000E..001F disallowed)
              skiptest "TODO"

          ptestCase "F13: Disallowed code points via Unicode escapes"
          <| fun () ->
              // INPUT: note "ok \\u{0}"
              // EXPECTED: PARSE ERROR or implementation-defined
              skiptest "TODO" ]

[<Tests>]
let numberTests =
    testList
        "G. Numbers (decimal, hex, octal, binary, underscores, exponent, keyword numbers)"
        [ test "G01: Decimal integer" {
              let node = parseSingleNode "n 123"
              let lit = node.Arguments |> List.head |> numberLiteral
              Expect.equal lit.Raw "123" "raw literal"
              Expect.equal lit.Kind NumberKind.Decimal "decimal kind"
          }

          test "G02: Decimal with fraction and exponent" {
              let node = parseSingleNode "n 1_234.50e+2"
              let lit = node.Arguments |> List.head |> numberLiteral
              Expect.equal lit.Raw "1_234.50e+2" "raw literal preserved"
              Expect.equal lit.Kind NumberKind.Decimal "still decimal"
          }

          test "G03: Binary / octal / hex" {
              let node = parseSingleNode "nums 0b1010 0o17 0xFF"
              let lits = node.Arguments |> List.map numberLiteral
              let kinds = lits |> List.map (fun l -> l.Kind)

              Expect.equal
                  kinds
                  [ NumberKind.Binary; NumberKind.Octal; NumberKind.Hexadecimal ]
                  "base-prefixed kinds detected"
          }

          test "G04: Leading decimal point is illegal" { expectParseError "n .1" }

          test "G05: Keyword numbers" {
              let node = parseSingleNode "n #inf #nan #-inf"

              let kinds =
                  node.Arguments
                  |> List.map (fun v ->
                      match numberLiteral v with
                      | { Kind = NumberKind.Special special } -> special
                      | lit -> failtestf "Expected special number, got %A" lit.Kind)

              Expect.equal
                  kinds
                  [ SpecialNumberKind.Infinity
                    SpecialNumberKind.NotANumber
                    SpecialNumberKind.NegativeInfinity ]
                  "keyword numbers preserved"
          }

          ptestCase "G06: Bare identifier 'inf' is illegal as an identifier string"
          <| fun () ->
              // INPUT: inf
              // EXPECTED: PARSE ERROR
              skiptest "TODO" ]

[<Tests>]
let booleanAndNullTests =
    testList
        "H. Booleans and null"
        [ test "H01: Boolean literals" {
              let node = parseSingleNode "flags #true enabled=#false"

              match node.Arguments with
              | Value.Boolean true :: _ -> ()
              | other -> failtestf "Expected boolean arg, got %A" other

              let props = propMap node

              match Map.tryFind "enabled" props with
              | Some(Value.Boolean false) -> ()
              | other -> failtestf "Expected enabled=#false, got %A" other
          }

          test "H02: Null literal" {
              let node = parseSingleNode "maybe #null value=#null"

              match node.Arguments with
              | Value.Null :: _ -> ()
              | other -> failtestf "Expected #null argument, got %A" other

              let props = propMap node

              match Map.tryFind "value" props with
              | Some Value.Null -> ()
              | other -> failtestf "Expected value=#null, got %A" other
          } ]

[<Tests>]
let newlineTests =
    testList
        "I. Newlines (CRLF handling) and multi-line string newline normalization"
        [ test "I01: CRLF is a single newline for node termination" {
              let nodes = parseNodes "zilker\r\npease\r\n"
              Expect.equal (List.map (fun n -> n.Name) nodes) [ "zilker"; "pease" ] "CRLF handled"
          }

          test "I02: Multi-line string literal newlines normalized to LF" {
              let node = parseSingleNode "s \"\"\"\na\r\nb\r\n\"\"\""
              let text = node.Arguments |> List.head |> stringOfValue
              Expect.equal text "a\nb" "CRLF normalized inside multiline"
          } ]

[<Tests>]
let childrenBlockTests =
    testList
        "J. Children blocks / hierarchy"
        [ test "J01: Simple hierarchy" {
              let node = parseSingleNode "parks {\npark \"Zilker\"\npark \"Pease\"\n}"
              Expect.equal (node.Children |> List.map (fun n -> n.Name)) [ "park"; "park" ] "two child nodes"

              let childArgs =
                  node.Children
                  |> List.map (fun n -> n.Arguments |> List.map stringOfValue |> List.head)

              Expect.equal childArgs [ "Zilker"; "Pease" ] "child arguments captured"
          }

          test "J02: Nested hierarchy with mixed entries" {
              let node =
                  parseSingleNode "park \"Zilker\" open=#true {\namenity \"pool\"\namenity name=\"Barton Springs\"\n}"

              let childNames = node.Children |> List.map (fun n -> n.Name)
              Expect.equal childNames [ "amenity"; "amenity" ] "two amenity children"
              let props = propMap node

              match Map.tryFind "open" props with
              | Some(Value.Boolean true) -> ()
              | other -> failtestf "Expected open property, got %A" other
          }

          test "J03: Children blocks can be on same line if nodes are terminated with semicolons" {
              let node = parseSingleNode "parks { park \"Zilker\"; park \"Pease\"; }"
              Expect.equal node.Children.Length 2 "two children on single line"
          } ]

[<Tests>]
let negativeSyntaxTests =
    testList
        "K. Targeted negative syntax tests (fast failure)"
        [ test "K01: Unterminated quoted string" { expectParseError "note \"oops" }

          ptestCase "K02: Unterminated multi-line comment"
          <| fun () ->
              // INPUT: /* oops\nzilker
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          test "K03: Missing closing brace in children block" { expectParseError "parks { zilker" }

          test "K04: Property missing value" { expectParseError "park name=" }

          test "K05: Property key must be a string (not a bare keyword)" { expectParseError "park #true=1" }

          test "K06: Disallowed literal BOM except at start of document" {
              let input = "zilker \uFEFF pease"
              expectParseError input
          }

          test "K07: Slashdash cannot be followed by another slashdash" { expectParseError "/- /- zilker" } ]

[<Tests>]
let roundTripTests =
    testList
        "L. 'Real-ish' Austin parks sample (round-trip smoke test)"
        [ ptestCase "L01: Parse + re-emit (serializer) should preserve semantics"
          <| fun () ->
              // INPUT: parks {\n    park "Zilker Metropolitan Park" area_acres=351 open=#true\n    ...\n}
              // EXPECTED: Parse and re-serialize should preserve semantics
              skiptest "TODO" ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
