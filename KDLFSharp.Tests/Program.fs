open Expecto
open KDLFSharp.Core

/// Helper to stringify token list (for snapshot comparison)
let private showTokens toks =
    toks |> List.map string |> String.concat " "

/// Run lexer on input and return stringified tokens
let private lex input = Lexer.tokenize input |> showTokens

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

          // TODO: add multiline string tests
          test "TODO multiline and raw strings" { Expect.isTrue true "placeholder for raw/multiline" } ]

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

          ptestCase "B02: Multi-line comment is whitespace (and may be nested)"
          <| fun () ->
              // INPUT: /* outer /* inner */ still outer */ zilker
              // EXPECTED: AST = [zilker]
              // TODO: Nested block comments not yet supported in lexer
              skiptest "Nested block comments not implemented"

          test "B03: Slashdash entire node (node is removed)" {
              let input = "/- zilker\npease"
              let result = Parser.parse input

              match result with
              | Ok nodes ->
                  Expect.hasLength nodes 1 "should have 1 node (zilker removed)"
                  Expect.equal nodes[0].Name "pease" "remaining node"
              | Error errs -> failtestf "Parse failed: %A" errs
          }

          ptestCase "B04: Slashdash argument removes that argument only"
          <| fun () ->
              // INPUT: trail "Lady Bird Lake" /- 3.1 miles open=#true
              // EXPECTED: trail.args = ["Lady Bird Lake"], trail.props.open = #true
              // TODO: Per-entry slashdash needs refinement in parser
              skiptest "Slashdash on individual arguments not fully implemented"

          ptestCase "B05: Slashdash property removes key+value"
          <| fun () ->
              // INPUT: park name="Zilker" /- hidden=#true open=#true
              // EXPECTED: park.props = {name:"Zilker", open:#true}
              // TODO: Per-entry slashdash needs refinement in parser
              skiptest "Slashdash on individual properties not fully implemented"

          ptestCase "B06: Slashdash 'just the property value' is illegal"
          <| fun () ->
              // INPUT: park hidden=/- #true
              // EXPECTED: PARSE ERROR
              // TODO: Need to detect and reject this syntax
              skiptest "Slashdash validation not implemented"

          ptestCase "B07: Slashdash children block removes the whole block"
          <| fun () ->
              // INPUT: park "Zilker" /- { note "should vanish" }
              // EXPECTED: park.children = []
              // TODO: Slashdash on children blocks needs implementation
              skiptest "Slashdash on children blocks not implemented"

          ptestCase "B08: After a slashdashed children block, only (other) children blocks may follow"
          <| fun () ->
              // INPUT: park "Zilker" /- { a } note "nope"
              // EXPECTED: PARSE ERROR
              // TODO: Need to enforce this constraint
              skiptest "Slashdashed children block constraint not enforced" ]

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
        [ ptestCase "D01: Arguments preserve relative order even when properties are interleaved"
          <| fun () ->
              // INPUT: foo 1 a=10 3 b=20 5
              // EXPECTED: foo.args = [1,3,5], foo.props = {a:10,b:20}
              skiptest "TODO"

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
        [ ptestCase "E01: Type annotation on node name"
          <| fun () ->
              // INPUT: (published)date "1970-01-01"
              // EXPECTED: node.name="date", node.type="published", args=["1970-01-01"]
              skiptest "TODO"

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

          ptestCase "E04: Slashdash can appear before a type annotation it is commenting out"
          <| fun () ->
              // INPUT: park /- (tag)"experimental" open=#true
              // EXPECTED: park.props.open=#true, typed string argument is absent
              skiptest "TODO" ]

[<Tests>]
let stringTests =
    testList
        "F. Strings (identifier, quoted, multiline, raw) + escapes"
        [ ptestCase "F01: Identifier string node name"
          <| fun () ->
              // INPUT: zilker
              // EXPECTED: node.name="zilker"
              skiptest "TODO"

          ptestCase "F02: Quoted string node name (spaces)"
          <| fun () ->
              // INPUT: "zilker park"
              // EXPECTED: node.name="zilker park"
              skiptest "TODO"

          ptestCase "F03: Identifier strings cannot contain forbidden punctuation"
          <| fun () ->
              // INPUT: zilker#park
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "F04: Quoted string with common escapes"
          <| fun () ->
              // INPUT: note "Line1\\nLine2\\tTabbed"
              // EXPECTED: note.args[0] == "Line1\nLine2\tTabbed"
              skiptest "TODO"

          ptestCase "F05: Unicode escape in quoted string"
          <| fun () ->
              // INPUT: park name="Zilker \\u{26F2}"
              // EXPECTED: name == "Zilker ⛲" (U+26F2)
              skiptest "TODO"

          ptestCase "F06: Escaped whitespace inside quoted string is discarded"
          <| fun () ->
              // INPUT: msg "Hello \\    World"
              // EXPECTED: msg.args[0] == "Hello World"
              skiptest "TODO"

          ptestCase "F07: Multi-line string basic (dedent rule)"
          <| fun () ->
              // INPUT: desc \"\"\"\nFirst line\n  Indented\n\"\"\"
              // EXPECTED: desc.args[0] == "First line\n  Indented"
              skiptest "TODO"

          ptestCase "F08: Multi-line string cannot be single-line (illegal)"
          <| fun () ->
              // INPUT: desc \"\"\"nope\"\"\"
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "F09: Raw single-line string (no escapes processed)"
          <| fun () ->
              // INPUT: just #"\\n will be literal"#
              // EXPECTED: just.args[0] == "\\n will be literal"
              skiptest "TODO"

          ptestCase "F10: Raw string delimiter with multiple # (can contain quotes)"
          <| fun () ->
              // INPUT: raw ##"hello\\n\\r\\asd\"#world"##
              // EXPECTED: raw.args[0] == "hello\\n\\r\\asd\"#world"
              skiptest "TODO"

          ptestCase "F11: Raw multi-line string (no escapes; dedent rules)"
          <| fun () ->
              // INPUT: rawml #\"\"\"\nHere's a \"\"\"\nmultiline string\n\"\"\"#
              // EXPECTED: literal triple-quotes text without escapes
              skiptest "TODO"

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
        [ ptestCase "G01: Decimal integer"
          <| fun () ->
              // INPUT: n 123
              // EXPECTED: n.args[0] == 123
              skiptest "TODO"

          ptestCase "G02: Decimal with fraction and exponent"
          <| fun () ->
              // INPUT: n 1_234.50e+2
              // EXPECTED: n.args[0] == 1234.50e+2 (underscores ignored)
              skiptest "TODO"

          ptestCase "G03: Binary / octal / hex"
          <| fun () ->
              // INPUT: nums 0b1010 0o17 0xFF
              // EXPECTED: args == [10, 15, 255]
              skiptest "TODO"

          ptestCase "G04: Leading decimal point is illegal"
          <| fun () ->
              // INPUT: n .1
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "G05: Keyword numbers"
          <| fun () ->
              // INPUT: n #inf #nan #-inf
              // EXPECTED: args == [#inf, #nan, #-inf]
              skiptest "TODO"

          ptestCase "G06: Bare identifier 'inf' is illegal as an identifier string"
          <| fun () ->
              // INPUT: inf
              // EXPECTED: PARSE ERROR
              skiptest "TODO" ]

[<Tests>]
let booleanAndNullTests =
    testList
        "H. Booleans and null"
        [ ptestCase "H01: Boolean literals"
          <| fun () ->
              // INPUT: flags #true enabled=#false
              // EXPECTED: flags.args[0]=#true, flags.props.enabled=#false
              skiptest "TODO"

          ptestCase "H02: Null literal"
          <| fun () ->
              // INPUT: maybe #null value=#null
              // EXPECTED: args[0]=#null, props.value=#null
              skiptest "TODO" ]

[<Tests>]
let newlineTests =
    testList
        "I. Newlines (CRLF handling) and multi-line string newline normalization"
        [ ptestCase "I01: CRLF is a single newline for node termination"
          <| fun () ->
              // INPUT: zilker<CRLF>pease<CRLF>
              // EXPECTED: AST = [zilker, pease]
              skiptest "TODO"

          ptestCase "I02: Multi-line string literal newlines normalized to LF"
          <| fun () ->
              // INPUT: s \"\"\"\na[CRLF]\nb[CRLF]\n\"\"\"
              // EXPECTED: s.args[0] == "a\nb"
              skiptest "TODO" ]

[<Tests>]
let childrenBlockTests =
    testList
        "J. Children blocks / hierarchy"
        [ ptestCase "J01: Simple hierarchy"
          <| fun () ->
              // INPUT: parks {\npark "Zilker"\npark "Pease"\n}
              // EXPECTED: parks.children = [park("Zilker"), park("Pease")]
              skiptest "TODO"

          ptestCase "J02: Nested hierarchy with mixed entries"
          <| fun () ->
              // INPUT: park "Zilker" open=#true {\namenity "pool" name="Barton Springs"\n}
              // EXPECTED: park.args=["Zilker"], park.props.open=#true, 2 children
              skiptest "TODO"

          ptestCase "J03: Children blocks can be on same line if nodes are terminated with semicolons"
          <| fun () ->
              // INPUT: parks { park "Zilker"; park "Pease"; }
              // EXPECTED: parks.children length == 2
              skiptest "TODO" ]

[<Tests>]
let negativeSyntaxTests =
    testList
        "K. Targeted negative syntax tests (fast failure)"
        [ ptestCase "K01: Unterminated quoted string"
          <| fun () ->
              // INPUT: note "oops
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K02: Unterminated multi-line comment"
          <| fun () ->
              // INPUT: /* oops\nzilker
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K03: Missing closing brace in children block"
          <| fun () ->
              // INPUT: parks { zilker
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K04: Property missing value"
          <| fun () ->
              // INPUT: park name=
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K05: Property key must be a string (not a bare keyword)"
          <| fun () ->
              // INPUT: park #true=1
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K06: Disallowed literal BOM except at start of document"
          <| fun () ->
              // INPUT: zilker <BOM> pease
              // EXPECTED: PARSE ERROR
              skiptest "TODO"

          ptestCase "K07: Slashdash cannot be followed by another slashdash"
          <| fun () ->
              // INPUT: /- /- zilker
              // EXPECTED: PARSE ERROR
              skiptest "TODO" ]

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
