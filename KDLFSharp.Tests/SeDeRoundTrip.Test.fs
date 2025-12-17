namespace KDLFSharp.Tests.SeDeRoundTrip

open Expecto
open KDLFSharp.Core.Serialize
open KDLFSharp.Core.Deserialize

module SeDeRoundTripTests =

    /// Simple record with primitive types
    type Person =
        { Name: string
          Age: int
          Height: float
          IsActive: bool }

    /// Record with optional fields
    type OptionalPerson =
        { Name: string
          Age: int option
          Email: string option }

    /// Nested record structure
    type Address =
        { Street: string
          City: string
          ZipCode: string }

    type PersonWithAddress =
        { Name: string
          Age: int
          Address: Address }

    /// Record with lists
    type Team =
        { Name: string
          Members: string list
          Scores: int array }

    /// Record with list of nested records
    type Company =
        { Name: string; Employees: Person list }

    /// Edge case: empty lists and optional None values
    type EdgeCaseRecord =
        { Id: int
          Tags: string list
          Description: string option
          Metadata: string option }

    /// Record with various numeric types
    type NumericRecord =
        { ByteValue: byte
          ShortValue: int16
          IntValue: int
          LongValue: int64
          FloatValue: float32
          DoubleValue: float
          DecimalValue: decimal }

    /// Record with special float values
    type SpecialFloats =
        { PositiveInfinity: float
          NegativeInfinity: float
          NotANumber: float }

    [<Tests>]
    let primitiveTests =
        testList
            "Primitive round-trip"
            [ testCase "Serialize and deserialize simple record"
              <| fun () ->
                  let person =
                      { Name = "Bob"
                        Age = 25
                        Height = 6.1
                        IsActive = false }

                  match toNode SerializeConfig.Default person with
                  | Ok node ->
                      match fromNode<Person> SerializeConfig.Default node with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name person.Name "Name should match"
                          Expect.equal deserialized.Age person.Age "Age should match"
                          Expect.equal deserialized.Height person.Height "Height should match"
                          Expect.equal deserialized.IsActive person.IsActive "IsActive should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Round-trip through KDL string"
              <| fun () ->
                  let person =
                      { Name = "Charlie"
                        Age = 35
                        Height = 5.9
                        IsActive = true }

                  match toString SerializeConfig.Default person with
                  | Ok kdl ->
                      match fromString<Person> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name person.Name "Name should match"
                          Expect.equal deserialized.Age person.Age "Age should match"
                          Expect.floatClose Accuracy.high deserialized.Height person.Height "Height should match"
                          Expect.equal deserialized.IsActive person.IsActive "IsActive should match"
                      | Error e -> failtestf "Deserialization from string failed: %O" e
                  | Error e -> failtestf "Serialization to string failed: %O" e ]

    [<Tests>]
    let optionalFieldTests =
        testList
            "Optional field round-trip"
            [ testCase "Round-trip with mixed Some/None values"
              <| fun () ->
                  let person =
                      { Name = "Charlie"
                        Age = Some 25
                        Email = None }

                  match toString SerializeConfig.Default person with
                  | Ok kdl ->
                      match fromString<OptionalPerson> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name person.Name "Name should match"
                          Expect.equal deserialized.Age person.Age "Age should match"
                          Expect.isNone deserialized.Email "Email should be None"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let nestedRecordTests =
        testList
            "Nested record round-trip"
            [ testCase "Round-trip nested record"
              <| fun () ->
                  let person =
                      { PersonWithAddress.Name = "Bob"
                        Age = 25
                        Address =
                          { Street = "456 Oak Ave"
                            City = "Dallas"
                            ZipCode = "75201" } }

                  match toString SerializeConfig.Default person with
                  | Ok kdl ->
                      match fromString<PersonWithAddress> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name person.Name "Name should match"
                          Expect.equal deserialized.Age person.Age "Age should match"
                          Expect.equal deserialized.Address.Street person.Address.Street "Street should match"
                          Expect.equal deserialized.Address.City person.Address.City "City should match"
                          Expect.equal deserialized.Address.ZipCode person.Address.ZipCode "ZipCode should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let listTests =
        testList
            "List and array round-trip"
            [ testCase "Round-trip list of primitives"
              <| fun () ->
                  let team =
                      { Name = "Sales"
                        Members = [ "David"; "Eve" ]
                        Scores = [| 88; 91 |] }

                  match toString SerializeConfig.Default team with
                  | Ok kdl ->
                      match fromString<Team> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name team.Name "Name should match"
                          Expect.equal deserialized.Members.Length team.Members.Length "Members count should match"
                          Expect.equal deserialized.Scores.Length team.Scores.Length "Scores count should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let nestedListTests =
        testList
            "List of nested records round-trip"
            [ testCase "Round-trip list of nested records"
              <| fun () ->
                  let company =
                      { Name = "StartupInc"
                        Employees =
                          [ { Name = "Charlie"
                              Age = 28
                              Height = 5.9
                              IsActive = true } ] }

                  match toString SerializeConfig.Default company with
                  | Ok kdl ->
                      match fromString<Company> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name company.Name "Company name should match"
                          Expect.equal deserialized.Employees.Length 1 "Should have 1 employee"
                          Expect.equal deserialized.Employees.Head.Name "Charlie" "Employee name should match"
                          Expect.equal deserialized.Employees.Head.Age 28 "Employee age should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let edgeCaseTests =
        testList
            "Edge cases and special values round-trip"
            [ testCase "Empty lists and None optionals"
              <| fun () ->
                  let record =
                      { Id = 42
                        Tags = []
                        Description = None
                        Metadata = None }

                  match toString SerializeConfig.Default record with
                  | Ok kdl ->
                      match fromString<EdgeCaseRecord> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Id record.Id "Id should match"
                          Expect.equal deserialized.Tags.Length 0 "Tags should be empty"
                          Expect.isNone deserialized.Description "Description should be None"
                          Expect.isNone deserialized.Metadata "Metadata should be None"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "All numeric types"
              <| fun () ->
                  let record =
                      { ByteValue = 255uy
                        ShortValue = 32767s
                        IntValue = 2147483647
                        LongValue = 9223372036854775807L
                        FloatValue = 3.14f
                        DoubleValue = 2.71828
                        DecimalValue = 1234.5678M }

                  match toString SerializeConfig.Default record with
                  | Ok kdl ->
                      match fromString<NumericRecord> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.ByteValue record.ByteValue "ByteValue should match"
                          Expect.equal deserialized.ShortValue record.ShortValue "ShortValue should match"
                          Expect.equal deserialized.IntValue record.IntValue "IntValue should match"
                          Expect.equal deserialized.LongValue record.LongValue "LongValue should match"

                          Expect.floatClose
                              Accuracy.medium
                              (float deserialized.FloatValue)
                              (float record.FloatValue)
                              "FloatValue should match"

                          Expect.floatClose
                              Accuracy.high
                              deserialized.DoubleValue
                              record.DoubleValue
                              "DoubleValue should match"

                          Expect.equal deserialized.DecimalValue record.DecimalValue "DecimalValue should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Special float values (infinity and NaN)"
              <| fun () ->
                  let record =
                      { PositiveInfinity = System.Double.PositiveInfinity
                        NegativeInfinity = System.Double.NegativeInfinity
                        NotANumber = System.Double.NaN }

                  match toString SerializeConfig.Default record with
                  | Ok kdl ->
                      Expect.isTrue (kdl.Contains("#inf")) "KDL should contain #inf"
                      Expect.isTrue (kdl.Contains("#-inf")) "KDL should contain #-inf"
                      Expect.isTrue (kdl.Contains("#nan")) "KDL should contain #nan"

                      match fromString<SpecialFloats> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.isTrue
                              (System.Double.IsPositiveInfinity deserialized.PositiveInfinity)
                              "PositiveInfinity should be infinity"

                          Expect.isTrue
                              (System.Double.IsNegativeInfinity deserialized.NegativeInfinity)
                              "NegativeInfinity should be negative infinity"

                          Expect.isTrue (System.Double.IsNaN deserialized.NotANumber) "NotANumber should be NaN"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "String with special characters"
              <| fun () ->
                  let person =
                      { Name = "Alice \"Wonder\" O'Brien\n\tTab"
                        Age = 30
                        Height = 5.6
                        IsActive = true }

                  match toString SerializeConfig.Default person with
                  | Ok kdl ->
                      match fromString<Person> SerializeConfig.Default kdl with
                      | Ok deserialized ->
                          Expect.equal deserialized.Name person.Name "Name with special chars should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Very long strings"
              <| fun () ->
                  let longString = System.String('x', 10000)

                  let person =
                      { Name = longString
                        Age = 30
                        Height = 5.6
                        IsActive = true }

                  match toString SerializeConfig.Default person with
                  | Ok kdl ->
                      match fromString<Person> SerializeConfig.Default kdl with
                      | Ok deserialized -> Expect.equal deserialized.Name.Length 10000 "Long string should match"
                      | Error e -> failtestf "Deserialization failed: %O" e
                  | Error e -> failtestf "Serialization failed: %O" e ]
