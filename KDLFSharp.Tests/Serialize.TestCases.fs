namespace KDLFSharp.Tests.Serialize

open KDLFSharp.Core.IR

/// Intermediate Representation (IR) for KDL conversion tests.
/// This IR is designed to be expressible in JSON/YAML/XML and provides a canonical format for round-trip conversion testing.
module TestCases =

    /// Test case definition
    type TestCase =
        { Id: string
          Description: string
          KdlInput: string
          ExpectedIR: IRDocument
          CanonicalKdl: string option
          XmlInput: string option
          JsonInput: string option
          YamlInput: string option }


    let T001 =
        { Id = "T001"
          Description = "Minimal node (no args/props/children)"
          KdlInput = "hello"
          ExpectedIR = [ node "hello" None [] Map.empty [] ]
          CanonicalKdl = Some "hello"
          XmlInput = Some "<doc>\n  <node name=\"hello\"/>\n</doc>"
          JsonInput = None
          YamlInput = None }

    let T002 =
        { Id = "T002"
          Description = "Args + props ordering (args keep order; props are a map)"
          KdlInput = "park \"Zilker\" 274.6150 open=#true city=\"Austin\""
          ExpectedIR =
            [ node
                  "park"
                  None
                  [ stringValue "Zilker"; numberValue 274.6150 ]
                  (Map.ofList [ ("open", boolValue true); ("city", stringValue "Austin") ])
                  [] ]
          CanonicalKdl = Some "park \"Zilker\" 274.6150 city=\"Austin\" open=#true"
          XmlInput =
            Some
                "<doc>\n  <node name=\"park\">\n    <args>\n      <arg k=\"string\" v=\"Zilker\"/>\n      <arg k=\"number\" v=\"274.6150\"/>\n    </args>\n    <props>\n      <prop key=\"open\" k=\"bool\" v=\"true\"/>\n      <prop key=\"city\" k=\"string\" v=\"Austin\"/>\n    </props>\n  </node>\n</doc>"
          JsonInput = None
          YamlInput = None }

    let T003 =
        { Id = "T003"
          Description = "Children hierarchy"
          KdlInput = "parks {\npark \"Zilker\"\npark \"Pease\"\n}"
          ExpectedIR =
            [ node
                  "parks"
                  None
                  []
                  Map.empty
                  [ node "park" None [ stringValue "Zilker" ] Map.empty []
                    node "park" None [ stringValue "Pease" ] Map.empty [] ] ]
          CanonicalKdl = Some "parks {\npark \"Zilker\"\npark \"Pease\"\n}"
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T004 =
        { Id = "T004"
          Description = "Node type annotation + typed value"
          KdlInput = "(published)dataset \"austin-parks\" generated_at=(date)\"2025-12-16\""
          ExpectedIR =
            [ node
                  "dataset"
                  (Some "published")
                  [ stringValue "austin-parks" ]
                  (Map.ofList [ ("generated_at", typedStringValue "date" "2025-12-16") ])
                  [] ]
          CanonicalKdl = Some "(published)dataset \"austin-parks\" generated_at=(date)\"2025-12-16\""
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T005 =
        { Id = "T005"
          Description = "Keyword numbers (#inf/#nan) via IR kw encoding"
          KdlInput = "nums #inf #-inf #nan"
          ExpectedIR =
            [ node "nums" None [ keywordValue "#inf"; keywordValue "#-inf"; keywordValue "#nan" ] Map.empty [] ]
          CanonicalKdl = Some "nums #inf #-inf #nan"
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T006 =
        { Id = "T006"
          Description = "Multiline strings (normalize semantics, not formatting)"
          KdlInput = "note \"\"\"\nLine 1\nLine 2\n\"\"\""
          ExpectedIR = [ node "note" None [ stringValue "Line 1\nLine 2" ] Map.empty [] ]
          CanonicalKdl = Some "note \"Line 1\\nLine 2\""
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T007 =
        { Id = "T007"
          Description = "Raw strings: keep literal backslashes"
          KdlInput = "path #\"C:\\data\\austin\\parks\"#"
          ExpectedIR = [ node "path" None [ stringValue "C:\\data\\austin\\parks" ] Map.empty [] ]
          CanonicalKdl = Some "path \"C:\\data\\austin\\parks\""
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T008 =
        { Id = "T008"
          Description = "Slashdash: semantic removal (parse ignores removed parts)"
          KdlInput =
            "parks {\npark \"Zilker\"\n/- park \"SHOULD_NOT_APPEAR\"\npark \"Pease\" /- open=#false open=#true\n}"
          ExpectedIR =
            [ node
                  "parks"
                  None
                  []
                  Map.empty
                  [ node "park" None [ stringValue "Zilker" ] Map.empty []
                    node "park" None [ stringValue "Pease" ] (Map.ofList [ ("open", boolValue true) ]) [] ] ]
          CanonicalKdl = Some "parks {\npark \"Zilker\"\npark \"Pease\" open=#true\n}"
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    let T009 =
        { Id = "T009"
          Description = "Natural XML element mapping is NOT used (must use IR mapping)"
          KdlInput = ""
          ExpectedIR = [ node "park" None [ stringValue "Zilker" ] (Map.ofList [ ("open", boolValue true) ]) [] ]
          CanonicalKdl = Some "park \"Zilker\" open=#true"
          XmlInput =
            Some
                "<doc>\n  <node name=\"park\">\n    <args>\n      <arg k=\"string\" v=\"Zilker\"/>\n    </args>\n    <props>\n      <prop key=\"open\" k=\"bool\" v=\"true\"/>\n    </props>\n  </node>\n</doc>"
          JsonInput = None
          YamlInput = None }

    let T010 =
        { Id = "T010"
          Description = "JSON object key ordering must be irrelevant"
          KdlInput = ""
          ExpectedIR = [ node "park" None [ stringValue "Zilker" ] (Map.ofList [ ("open", boolValue true) ]) [] ]
          CanonicalKdl = Some "park \"Zilker\" open=#true"
          XmlInput = None
          JsonInput =
            Some
                "[\n    {\n        \"children\": [],\n        \"props\": { \"open\": { \"v\": true, \"k\": \"bool\", \"t\": null } },\n        \"args\": [ { \"v\": \"Zilker\", \"k\": \"string\", \"t\": null } ],\n        \"type\": null,\n        \"name\": \"park\"\n    }\n]"
          YamlInput = None }

    let T011 =
        { Id = "T011"
          Description = "YAML must be parsed as YAML 1.2 (JSON compatibility expectations)"
          KdlInput = ""
          ExpectedIR =
            [ node
                  "park"
                  None
                  [ stringValue "Zilker"; numberValue 274.615 ]
                  (Map.ofList [ ("open", boolValue true) ])
                  [] ]
          CanonicalKdl = Some "park \"Zilker\" 274.615 open=#true"
          XmlInput = None
          JsonInput = None
          YamlInput =
            Some
                "- name: park\n  type: null\n  args:\n    - { t: null, k: string, v: Zilker }\n    - { t: null, k: number, v: 274.615 }\n  props:\n    open: { t: null, k: bool, v: true }\n  children: []" }

    let T012 =
        { Id = "T012"
          Description = "Round-trip stability: KDL -> JSON -> KDL (canonical)"
          KdlInput = "(published)parks \"Austin\" {\npark \"Zilker\" acres=(f64)274.6150 open=#true\n}"
          ExpectedIR =
            [ node
                  "parks"
                  (Some "published")
                  [ stringValue "Austin" ]
                  Map.empty
                  [ node
                        "park"
                        None
                        [ stringValue "Zilker" ]
                        (Map.ofList [ ("acres", typedNumberValue "f64" 274.6150); ("open", boolValue true) ])
                        [] ] ]
          CanonicalKdl = Some "(published)parks \"Austin\" {\npark \"Zilker\" acres=(f64)274.6150 open=#true\n}"
          XmlInput = None
          JsonInput = None
          YamlInput = None }

    /// All positive test cases
    let AllTests =
        [ T001; T002; T003; T004; T005; T006; T007; T008; T009; T010; T011; T012 ]

    /// Negative test cases
    module Negative =
        type NegativeTestCase =
            { Id: string
              Description: string
              Input: string
              Format: string // "JSON", "XML", "YAML", "KDL"
              ExpectedErrorPattern: string }

        let N001 =
            { Id = "N001"
              Description = "JSON not matching IR schema"
              Input = "{ \"name\": \"park\" }"
              Format = "JSON"
              ExpectedErrorPattern = "document must be an array" }

        let N002 =
            { Id = "N002"
              Description = "XML missing required root wrapper"
              Input = "<node name=\"park\"/>"
              Format = "XML"
              ExpectedErrorPattern = "document must be rooted at <doc>" }

        let N003 =
            { Id = "N003"
              Description = "XML arg missing k attribute"
              Input = "<doc>\n  <node name=\"park\">\n    <args><arg v=\"Zilker\"/></args>\n  </node>\n</doc>"
              Format = "XML"
              ExpectedErrorPattern = "arg must include k" }

        let AllNegativeTests = [ N001; N002; N003 ]
