namespace KDLFSharp.Tests.Serialize

open Expecto
open KDLFSharp.Tests.Serialize.TestCases

module JSONTests =

    /// Parse JSON input into IR document
    let parseJSON (json: string) : Result<IRDocument, string> =
        // TODO: Implement JSON -> IR parser
        Error "JSON parsing not yet implemented"

    /// Emit IR document as JSON
    let emitJSON (ir: IRDocument) : string =
        // TODO: Implement IR -> JSON emitter
        failwith "JSON emission not yet implemented"

    [<Tests>]
    let jsonParseTests =
        testList
            "JSON Parse Tests"
            [ ptestCase "T001: Parse minimal node from JSON"
              <| fun () ->
                  let json =
                      "[
    {
        \"name\": \"hello\",
        \"type\": null,
        \"args\": [],
        \"props\": {},
        \"children\": []
    }
]"

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T001.ExpectedIR "IR should match expected"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "T002: Parse args + props from JSON"
              <| fun () ->
                  let json =
                      "[
    {
    \"name\": \"park\",
    \"type\": null,
    \"args\": [
        { \"t\": null, \"k\": \"string\", \"v\": \"Zilker\" },
        { \"t\": null, \"k\": \"number\", \"v\": 274.6150 }
    ],
    \"props\": {
        \"open\": { \"t\": null, \"k\": \"bool\", \"v\": true },
        \"city\": { \"t\": null, \"k\": \"string\", \"v\": \"Austin\" }
    },
    \"children\": []
    }
]"

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T002.ExpectedIR "IR should match expected"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "T003: Parse children hierarchy from JSON"
              <| fun () ->
                  match parseJSON T003.JsonInput.Value with
                  | Ok ir -> Expect.equal ir T003.ExpectedIR "IR should match expected"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "T004: Parse type annotations from JSON"
              <| fun () ->
                  let json =
                      "[
    {
        \"name\": \"dataset\",
        \"type\": \"published\",
        \"args\": [ { \"t\": null, \"k\": \"string\", \"v\": \"austin-parks\" } ],
        \"props\": {
            \"generated_at\": { \"t\": \"date\", \"k\": \"string\", \"v\": \"2025-12-16\" }
        },
        \"children\": []
    }
]"

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T004.ExpectedIR "IR should match expected"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "T005: Parse keyword numbers from JSON"
              <| fun () ->
                  let json =
                      "[
    {
        \"name\": \"nums\",
        \"type\": null,
        \"args\": [
            { \"t\": null, \"k\": \"kw\", \"v\": \"#inf\" },
            { \"t\": null, \"k\": \"kw\", \"v\": \"#-inf\" },
            { \"t\": null, \"k\": \"kw\", \"v\": \"#nan\" }
        ],
        \"props\": {},
        \"children\": []
    }
]"

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T005.ExpectedIR "IR should match expected"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "T010: JSON object key ordering must be irrelevant"
              <| fun () ->
                  match T010.JsonInput with
                  | Some json ->
                      match parseJSON json with
                      | Ok ir -> Expect.equal ir T010.ExpectedIR "IR should match expected despite key shuffling"
                      | Error err -> failtestf "Parse failed: %s" err
                  | None -> skiptest "No JSON input for T010" ]

    [<Tests>]
    let jsonEmitTests =
        testList
            "JSON Emit Tests"
            [ ptestCase "T001: Emit minimal node to JSON"
              <| fun () ->
                  let json = emitJSON T001.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T001.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T002: Emit args + props to JSON"
              <| fun () ->
                  let json = emitJSON T002.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T002.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T003: Emit children hierarchy to JSON"
              <| fun () ->
                  let json = emitJSON T003.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T003.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T004: Emit type annotations to JSON"
              <| fun () ->
                  let json = emitJSON T004.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T004.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T005: Emit keyword numbers to JSON"
              <| fun () ->
                  let json = emitJSON T005.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T005.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T006: Emit multiline strings to JSON"
              <| fun () ->
                  let json = emitJSON T006.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T006.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T007: Emit raw strings to JSON"
              <| fun () ->
                  let json = emitJSON T007.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T007.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T008: Emit after slashdash removal to JSON"
              <| fun () ->
                  let json = emitJSON T008.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T008.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T009: Emit IR mapping to JSON"
              <| fun () ->
                  let json = emitJSON T009.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T009.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T012: Round-trip stability test"
              <| fun () ->
                  let json = emitJSON T012.ExpectedIR

                  match parseJSON json with
                  | Ok ir -> Expect.equal ir T012.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err ]

    [<Tests>]
    let jsonNegativeTests =
        testList
            "JSON Negative Tests"
            [ ptestCase "N001: JSON not matching IR schema"
              <| fun () ->
                  match parseJSON Negative.N001.Input with
                  | Error msg -> Expect.stringContains msg "array" "Error should mention document must be an array"
                  | Ok _ -> failtest "Should have failed with error" ]

    [<Tests>]
    let jsonIntegrationTests =
        testList
            "JSON Integration Tests"
            [ ptestCase "Parse JSON with all features"
              <| fun () ->
                  let json =
                      "[
    {
        \"name\": \"parks\",
        \"type\": \"published\",
        \"args\": [ { \"t\": null, \"k\": \"string\", \"v\": \"Austin\" } ],
        \"props\": {},
        \"children\": [
            {
                \"name\": \"park\",
                \"type\": null,
                \"args\": [ { \"t\": null, \"k\": \"string\", \"v\": \"Zilker\" } ],
                \"props\": {
                    \"acres\": { \"t\": \"f64\", \"k\": \"number\", \"v\": 274.6150 },
                    \"open\": { \"t\": null, \"k\": \"bool\", \"v\": true }
                },
                \"children\": []
            }
        ]
    }
]"

                  match parseJSON json with
                  | Ok ir ->
                      Expect.hasLength ir 1 "Should have one root node"
                      let root = ir[0]
                      Expect.equal root.Name "parks" "Root node name"
                      Expect.equal root.Type (Some "published") "Root node type"
                      Expect.hasLength root.Children 1 "Should have one child"

                      let child = root.Children[0]
                      Expect.equal child.Properties.Count 2 "Child should have 2 properties"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "Emit and re-parse preserves structure"
              <| fun () ->
                  let original = T012.ExpectedIR
                  let json = emitJSON original

                  match parseJSON json with
                  | Ok reparsed -> Expect.equal reparsed original "Round-trip should preserve all structure"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "JSON key ordering should not affect parsing"
              <| fun () ->
                  // Same IR but keys in different order
                  let json1 =
                      "[
    { \"name\": \"test\", \"type\": null, \"args\": [], \"props\": {}, \"children\": [] }
]"

                  let json2 =
                      "[
    { \"children\": [], \"props\": {}, \"args\": [], \"type\": null, \"name\": \"test\" }
]"

                  match parseJSON json1, parseJSON json2 with
                  | Ok ir1, Ok ir2 -> Expect.equal ir1 ir2 "Different key orders should parse to same IR"
                  | Error err, _ -> failtestf "Parse json1 failed: %s" err
                  | _, Error err -> failtestf "Parse json2 failed: %s" err ]

    [<Tests>]
    let jsonCanonicalFormatTests =
        testList
            "JSON Canonical Format Tests"
            [ ptestCase "Emitted JSON should have stable key ordering"
              <| fun () ->
                  let json = emitJSON T002.ExpectedIR
                  // JSON should have keys in order: name, type, args, props, children
                  let lines = json.Split('\n') |> Array.map (fun l -> l.Trim())

                  let nameIdx = lines |> Array.tryFindIndex (fun l -> l.StartsWith("\"name\""))

                  let typeIdx = lines |> Array.tryFindIndex (fun l -> l.StartsWith("\"type\""))

                  let argsIdx = lines |> Array.tryFindIndex (fun l -> l.StartsWith("\"args\""))

                  match nameIdx, typeIdx, argsIdx with
                  | Some n, Some t, Some a ->
                      Expect.isLessThan n t "name should come before type"
                      Expect.isLessThan t a "type should come before args"
                  | _ -> skiptest "Could not find expected keys in output"

              ptestCase "Emitted JSON should be valid JSON"
              <| fun () ->
                  let json = emitJSON T001.ExpectedIR
                  // Should be able to re-parse it
                  match parseJSON json with
                  | Ok _ -> ()
                  | Error err -> failtestf "Emitted JSON is not valid: %s" err ]
