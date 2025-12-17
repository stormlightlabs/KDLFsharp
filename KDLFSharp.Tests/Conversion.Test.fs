namespace KDLFSharp.Tests.Conversion

open Expecto
open KDLFSharp.Core

module ConversionTests =

    let private sampleDoc =
        [ { TypeAnn = Some "meta"
            Name = "dataset"
            Arguments =
              [ Value.String("austin-parks", Some "slug")
                Value.Number(NumberLiteral.ofRaw "0x2A", Some "i32")
                Value.Number(NumberLiteral.special "#inf" SpecialNumberKind.Infinity, None) ]
            Properties =
              [ { Key = "city"
                  Value = Value.String("Austin", None) }
                { Key = "open"
                  Value = Value.Boolean(true, Some "flag") }
                { Key = "note"
                  Value = Value.Null None } ]
            Children =
              [ { TypeAnn = None
                  Name = "park"
                  Arguments =
                    [ Value.String("Zilker Metropolitan Park", None)
                      Value.Number(NumberLiteral.ofRaw "274.6150", Some "f64") ]
                  Properties =
                    [ { Key = "off_leash"
                        Value = Value.Boolean(false, None) } ]
                  Children = []
                  Span = None
                  OriginalText = None } ]
            Span = None
            OriginalText = None } ]

    [<Tests>]
    let structuredJsonTests =
        testList
            "Structured JSON"
            [ testCase "Round-trip document"
              <| fun () ->
                  let json = StructuredJSON.emitDocument sampleDoc

                  match StructuredJSON.parseDocument json with
                  | Ok parsed -> Expect.equal parsed sampleDoc "JSON round-trip should preserve document"
                  | Error err -> failtestf "Failed to parse emitted JSON: %s" err

              testCase "Missing literal fails"
              <| fun () ->
                  let invalidJson =
                      """{"nodes":[{"name":"broken","typeAnnotation":null,"arguments":[{"kind":"number","typeAnnotation":null}],"properties":[],"children":[]}]}"""

                  match StructuredJSON.parseDocument invalidJson with
                  | Ok _ -> failtest "Expected parse to fail for missing literal"
                  | Error _ -> () ]

    [<Tests>]
    let structuredXmlTests =
        testList
            "Structured XML"
            [ testCase "Round-trip document"
              <| fun () ->
                  let xml = StructuredXML.emitDocument sampleDoc

                  match StructuredXML.parseDocument xml with
                  | Ok parsed -> Expect.equal parsed sampleDoc "XML round-trip should preserve document"
                  | Error err -> failtestf "Failed to parse emitted XML: %s" err

              testCase "Missing literal fails"
              <| fun () ->
                  let invalidXml =
                      "<kdl><node name=\"broken\"><arguments><value kind=\"number\"/></arguments></node></kdl>"

                  match StructuredXML.parseDocument invalidXml with
                  | Ok _ -> failtest "Expected XML parse to fail for missing literal"
                  | Error _ -> () ]

    [<Tests>]
    let sampleEmitTests =
        testList
            "Sample emitters"
            [ testCase "Sample JSON contains friendly layout"
              <| fun () ->
                  let json = SampleJSON.emitDocument sampleDoc
                  Expect.isTrue (json.Contains("\"dataset\"")) "dataset key present"
                  Expect.isTrue (json.Contains("\"_meta\"")) "metadata emitted"

              testCase "Sample XML contains metadata namespace"
              <| fun () ->
                  let xml = SampleXML.emitDocument sampleDoc
                  Expect.isTrue (xml.Contains("stormlightlabs")) "metadata namespace present"

              testCase "Sample JSON round-trips through parser"
              <| fun () ->
                  let json = SampleJSON.emitDocument sampleDoc

                  match SampleJSON.parseDocument json with
                  | Ok parsed ->
                      Expect.equal parsed.Length sampleDoc.Length "document node count preserved"
                      Expect.equal parsed.Head.Name sampleDoc.Head.Name "root name preserved"

                      Expect.equal
                          parsed.Head.Properties.Length
                          sampleDoc.Head.Properties.Length
                          "property count preserved"

                      Expect.equal
                          parsed.Head.Arguments.Length
                          sampleDoc.Head.Arguments.Length
                          "argument count preserved"
                  | Error err -> failtestf "Sample JSON parse failed: %s" err

              testCase "Sample XML round-trips through parser"
              <| fun () ->
                  let xml = SampleXML.emitDocument sampleDoc

                  match SampleXML.parseDocument xml with
                  | Ok parsed ->
                      Expect.equal parsed.Length sampleDoc.Length "document node count preserved"
                      Expect.equal parsed.Head.Name sampleDoc.Head.Name "root name preserved"

                      Expect.equal
                          parsed.Head.Properties.Length
                          sampleDoc.Head.Properties.Length
                          "property count preserved"

                      Expect.equal
                          parsed.Head.Arguments.Length
                          sampleDoc.Head.Arguments.Length
                          "argument count preserved"
                  | Error err -> failtestf "Sample XML parse failed: %s" err ]
