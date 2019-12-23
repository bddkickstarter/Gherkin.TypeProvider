module FSharp.Data.Gherkin.Tests.Example

open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers

open Expecto

[<Tests>]
let examples =

    let examples = TestFeature
                    .CreateFeature()
                    .Scenarios
                    .``Scenario outline name``
                    .Examples
    testList
        "Examples have correct data"
        [
            testCase
                "Correct number of examples"
                <| fun _ ->
                    Expect.isTrue (examples.Length = 3) (sprintf "Expected %i examples got %i" 3 examples.Length)

            testCase
                "Examples contain correct data"
                <| fun _ ->
                    validateData examples.[0].``Example Column 1`` "Example Column 1" "Set 1 Data 1"
                    validateData examples.[0].``Example Column 2`` "Example Column 2" "Set 1 Data 2"
                    validateData examples.[0].``Example Column 3`` "Example Column 3" "Set 1 Data 3"

                    validateData examples.[1].``Example Column 1`` "Example Column 1" "Set 2 Data 1"
                    validateData examples.[1].``Example Column 2`` "Example Column 2" "Set 2 Data 2"
                    validateData examples.[1].``Example Column 3`` "Example Column 3" "Set 2 Data 3"

                    validateData examples.[2].``Example Column 1`` "Example Column 1" "Set 3 Data 1"
                    validateData examples.[2].``Example Column 2`` "Example Column 2" "Set 3 Data 2"
                    validateData examples.[2].``Example Column 3`` "Example Column 3" "Set 3 Data 3"
        ]