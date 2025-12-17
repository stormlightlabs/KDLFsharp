namespace KDLFSharp.Tests.Serialize

open Expecto
open KDLFSharp.Core
open KDLFSharp.Core.Serialize

module SerializeTests =

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

    [<Tests>]
    let primitiveTests =
        testList
            "Primitive serialization"
            [ testCase "Serialize simple record with primitives"
              <| fun () ->
                  let person =
                      { Name = "Alice"
                        Age = 30
                        Height = 5.6
                        IsActive = true }

                  match toNode SerializeConfig.Default person with
                  | Ok node ->
                      Expect.equal node.Name "Person" "Node name should match record type"
                      Expect.equal node.Properties.Length 4 "Should have 4 properties"

                      let nameOpt = node.Properties |> List.tryFind (fun p -> p.Key = "Name")
                      Expect.isSome nameOpt "Name property should exist"

                      match nameOpt with
                      | Some { Value = Value.String(s, _) } -> Expect.equal s "Alice" "Name value should be Alice"
                      | _ -> failtest "Name should be a string value"
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let optionalFieldTests =
        testList
            "Optional field serialization"
            [ testCase "Serialize record with Some values"
              <| fun () ->
                  let person =
                      { Name = "Alice"
                        Age = Some 30
                        Email = Some "alice@example.com" }

                  match toNode SerializeConfig.Default person with
                  | Ok node ->
                      Expect.equal node.Properties.Length 3 "Should have 3 properties (Name, Age, Email)"

                      let ageOpt = node.Properties |> List.tryFind (fun p -> p.Key = "Age")
                      Expect.isSome ageOpt "Age property should exist"
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Serialize record with None values"
              <| fun () ->
                  let person =
                      { Name = "Bob"
                        Age = None
                        Email = None }

                  match toNode SerializeConfig.Default person with
                  | Ok node ->
                      Expect.equal node.Properties.Length 1 "Should have 1 property (only Name)"

                      Expect.isSome
                          (node.Properties |> List.tryFind (fun p -> p.Key = "Name"))
                          "Name property should exist"
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let nestedRecordTests =
        testList
            "Nested record serialization"
            [ testCase "Serialize nested record as child node"
              <| fun () ->
                  let person =
                      { PersonWithAddress.Name = "Alice"
                        Age = 30
                        Address =
                          { Street = "123 Main St"
                            City = "Austin"
                            ZipCode = "78701" } }

                  match toNode SerializeConfig.Default person with
                  | Ok node ->
                      Expect.equal node.Children.Length 1 "Should have 1 child node (Address)"
                      let addressNode = node.Children.Head
                      Expect.equal addressNode.Name "Address" "Child node should be named Address"
                      Expect.equal addressNode.Properties.Length 3 "Address should have 3 properties"
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let listTests =
        testList
            "List and array serialization"
            [ testCase "Serialize list as child nodes"
              <| fun () ->
                  let team =
                      { Name = "Engineering"
                        Members = [ "Alice"; "Bob"; "Charlie" ]
                        Scores = [| 95; 87; 92 |] }

                  match toNode SerializeConfig.Default team with
                  | Ok node ->
                      Expect.isGreaterThan node.Children.Length 0 "Should have children from lists"

                      Expect.equal
                          (node.Children |> List.filter (fun n -> n.Name = "Members")).Length
                          3
                          "Should have 3 Members nodes"
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Serialize empty lists"
              <| fun () ->
                  let team =
                      { Name = "EmptyTeam"
                        Members = []
                        Scores = [||] }

                  match toNode SerializeConfig.Default team with
                  | Ok node ->
                      Expect.equal node.Name "Team" "Node name should be Team"

                      Expect.equal
                          (node.Children |> List.filter (fun n -> n.Name = "Members")).Length
                          0
                          "Empty lists should not add child nodes"
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let nestedListTests =
        testList
            "List of nested records serialization"
            [ testCase "Serialize list of records as children"
              <| fun () ->
                  let company =
                      { Name = "TechCorp"
                        Employees =
                          [ { Name = "Alice"
                              Age = 30
                              Height = 5.6
                              IsActive = true }
                            { Name = "Bob"
                              Age = 25
                              Height = 6.1
                              IsActive = false } ] }

                  match toNode SerializeConfig.Default company with
                  | Ok node ->
                      Expect.equal node.Children.Length 1 "Should have 1 wrapper node (Employees)"

                      let employeesWrapper = node.Children.Head
                      Expect.equal employeesWrapper.Name "Employees" "Wrapper node should be named Employees"
                      Expect.equal employeesWrapper.Children.Length 2 "Wrapper should have 2 Person nodes"
                      Expect.equal employeesWrapper.Children.Head.Name "Person" "Child node should be named Person"
                  | Error e -> failtestf "Serialization failed: %O" e ]

    [<Tests>]
    let configTests =
        testList
            "Configuration options"
            [ testCase "Disable type annotations"
              <| fun () ->
                  let config =
                      { SerializeConfig.Default with
                          IncludeTypeAnnotations = false }

                  let person =
                      { Name = "Alice"
                        Age = 30
                        Height = 5.6
                        IsActive = true }

                  match toNode config person with
                  | Ok node ->
                      Expect.isNone node.TypeAnn "Node should not have type annotation"

                      node.Properties
                      |> List.iter (fun prop ->
                          match prop.Value with
                          | Value.String(_, ty)
                          | Value.Number(_, ty)
                          | Value.Boolean(_, ty)
                          | Value.Null ty -> Expect.isNone ty "Values should not have type annotations"
                          | _ -> ())
                  | Error e -> failtestf "Serialization failed: %O" e

              testCase "Custom root node name"
              <| fun () ->
                  let config =
                      { SerializeConfig.Default with
                          RootNodeName = Some "custom-person" }

                  let person =
                      { Name = "Bob"
                        Age = 25
                        Height = 6.1
                        IsActive = false }

                  match toNode config person with
                  | Ok node -> Expect.equal node.Name "custom-person" "Should use custom node name"
                  | Error e -> failtestf "Serialization failed: %O" e ]
