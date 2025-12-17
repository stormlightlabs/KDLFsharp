namespace KDLFSharp.Tests.Deserialize

open Expecto
open KDLFSharp.Core.Serialize
open KDLFSharp.Core.Deserialize

module DeserializeTests =

    /// Simple record with primitive types
    type Person =
        { Name: string
          Age: int
          Height: float
          IsActive: bool }

    [<Tests>]
    let errorHandlingTests =
        testList
            "Error handling"
            [ testCase "Deserialize with missing required field"
              <| fun () ->
                  let kdl =
                      """
Person
    Name="Alice"
    Age=30
"""

                  match fromString<Person> SerializeConfig.Default kdl with
                  | Ok _ -> failtest "Should fail due to missing Height and IsActive"
                  | Error e ->
                      match e with
                      | MissingRequiredField _ -> ()
                      | DeserializationFailed _ -> ()
                      | _ -> failtestf "Expected MissingRequiredField or DeserializationFailed error, got %O" e

              testCase "Deserialize empty document"
              <| fun () ->
                  match fromDocument<Person> SerializeConfig.Default [] with
                  | Ok _ -> failtest "Should fail with empty document"
                  | Error(InvalidValue _) -> ()
                  | Error e -> failtestf "Expected InvalidValue error, got %O" e

              testCase "Deserialize malformed KDL"
              <| fun () ->
                  let badKdl = "Person { Name="

                  match fromString<Person> SerializeConfig.Default badKdl with
                  | Ok _ -> failtest "Should fail with malformed KDL"
                  | Error(DeserializationFailed _) -> ()
                  | Error e -> failtestf "Expected DeserializationFailed error, got %O" e ]
