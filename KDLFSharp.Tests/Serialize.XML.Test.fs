namespace KDLFSharp.Tests.Serialize

open Expecto
open KDLFSharp.Tests.Serialize.TestCases

module XMLTests =

    /// Parse XML input into IR document
    let parseXML (xml: string) : Result<IRDocument, string> =
        // TODO: Implement XML -> IR parser
        Error "XML parsing not yet implemented"

    /// Emit IR document as XML
    let emitXML (ir: IRDocument) : string =
        // TODO: Implement IR -> XML emitter
        failwith "XML emission not yet implemented"

    [<Tests>]
    let xmlParseTests =
        testList
            "XML Parse Tests"
            [ ptestCase "T001: Parse minimal node from XML"
              <| fun () ->
                  match T001.XmlInput with
                  | Some xml ->
                      match parseXML xml with
                      | Ok ir -> Expect.equal ir T001.ExpectedIR "IR should match expected"
                      | Error err -> failtestf "Parse failed: %s" err
                  | None -> skiptest "No XML input for T001"

              ptestCase "T002: Parse args + props from XML"
              <| fun () ->
                  match T002.XmlInput with
                  | Some xml ->
                      match parseXML xml with
                      | Ok ir -> Expect.equal ir T002.ExpectedIR "IR should match expected"
                      | Error err -> failtestf "Parse failed: %s" err
                  | None -> skiptest "No XML input for T002"

              ptestCase "T009: XML must use IR mapping (not natural mapping)"
              <| fun () ->
                  match T009.XmlInput with
                  | Some xml ->
                      match parseXML xml with
                      | Ok ir -> Expect.equal ir T009.ExpectedIR "IR should match expected"
                      | Error err -> failtestf "Parse failed: %s" err
                  | None -> skiptest "No XML input for T009" ]

    [<Tests>]
    let xmlEmitTests =
        testList
            "XML Emit Tests"
            [ ptestCase "T001: Emit minimal node to XML"
              <| fun () ->
                  let xml = emitXML T001.ExpectedIR
                  // Re-parse and compare IR
                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T001.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T002: Emit args + props to XML"
              <| fun () ->
                  let xml = emitXML T002.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T002.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T003: Emit children hierarchy to XML"
              <| fun () ->
                  let xml = emitXML T003.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T003.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T004: Emit type annotations to XML"
              <| fun () ->
                  let xml = emitXML T004.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T004.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T005: Emit keyword numbers to XML"
              <| fun () ->
                  let xml = emitXML T005.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T005.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T006: Emit multiline strings to XML"
              <| fun () ->
                  let xml = emitXML T006.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T006.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T007: Emit raw strings to XML"
              <| fun () ->
                  let xml = emitXML T007.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T007.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T008: Emit after slashdash removal to XML"
              <| fun () ->
                  let xml = emitXML T008.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T008.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err

              ptestCase "T012: Round-trip stability test"
              <| fun () ->
                  let xml = emitXML T012.ExpectedIR

                  match parseXML xml with
                  | Ok ir -> Expect.equal ir T012.ExpectedIR "Round-trip IR should match"
                  | Error err -> failtestf "Re-parse failed: %s" err ]

    [<Tests>]
    let xmlNegativeTests =
        testList
            "XML Negative Tests"
            [ ptestCase "N002: XML missing required root wrapper"
              <| fun () ->
                  match parseXML Negative.N002.Input with
                  | Error msg -> Expect.stringContains msg "doc" "Error should mention missing <doc> root"
                  | Ok _ -> failtest "Should have failed with error"

              ptestCase "N003: XML arg missing k attribute"
              <| fun () ->
                  match parseXML Negative.N003.Input with
                  | Error msg -> Expect.stringContains msg "k" "Error should mention missing k attribute"
                  | Ok _ -> failtest "Should have failed with error" ]

    [<Tests>]
    let xmlIntegrationTests =
        testList
            "XML Integration Tests"
            [ ptestCase "Parse XML with all features"
              <| fun () ->
                  // Complex XML with nested children, type annotations, and various value types
                  let xml =
                      "<doc>
  <node name=\"parks\" type=\"published\">
    <args>
      <arg k=\"string\" v=\"Austin\"/>
    </args>
    <children>
      <node name=\"park\">
        <args>
          <arg k=\"string\" v=\"Zilker\"/>
        </args>
        <props>
          <prop key=\"acres\" t=\"f64\" k=\"number\" v=\"274.6150\"/>
          <prop key=\"open\" k=\"bool\" v=\"true\"/>
        </props>
      </node>
    </children>
  </node>
</doc>"

                  match parseXML xml with
                  | Ok ir ->
                      Expect.hasLength ir 1 "Should have one root node"
                      let root = ir[0]
                      Expect.equal root.Name "parks" "Root node name"
                      Expect.equal root.Type (Some "published") "Root node type"
                      Expect.hasLength root.Children 1 "Should have one child"
                  | Error err -> failtestf "Parse failed: %s" err

              ptestCase "Emit and re-parse preserves structure"
              <| fun () ->
                  let original = T012.ExpectedIR
                  let xml = emitXML original

                  match parseXML xml with
                  | Ok reparsed -> Expect.equal reparsed original "Round-trip should preserve all structure"
                  | Error err -> failtestf "Re-parse failed: %s" err ]
