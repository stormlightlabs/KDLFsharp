namespace KDLFSharp.Tests.Conversion

open Expecto
open KDLFSharp.Core
open KDLFSharp.Core.StructuredJSON
open KDLFSharp.Core.StructuredXML

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
