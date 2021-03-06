module FSharp.Data.Gherkin.Tests.ScenarioOutline

open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers
open Expecto

[<Tests>]
let scenarioOutline =

    let scenarioOutline = TestFeature.CreateFeature().Scenarios.``Scenario outline name``

    testList
        "Scenario Outline has correct data"
        [
            testCase
                "Scenario Outline is correct"
                <| fun _ -> validateScenario scenarioOutline "Scenario outline name" "Multi-line\r\nScenario Outline Description"

            testCase
                    "Scenario Outline tags correct"
                    <| fun _ ->
                        let tag1 = scenarioOutline.Tags.scenarioOutlineTag1.Name
                        let tag2 = scenarioOutline.Tags.scenarioOutlineTag2.Name

                        Expect.equal tag1 "@scenarioOutlineTag1" (sprintf "Expected tag name %s but got %s" "@scenarioOutlineTag1" tag1)
                        Expect.equal tag2 "@scenarioOutlineTag2" (sprintf "Expected tag name %s but got %s" "@scenarioOutlineTag2" tag2)

            testCase
                "Scenario Outline has correct number of steps"
                <| fun _ -> Expect.isTrue (scenarioOutline.Steps.Length = 3) (sprintf "Expected 3 steps but got %i" scenarioOutline.Steps.Length)

            testCase
                "Scenario Outline Given correct"
                <| fun _ ->

                    validateStep
                        scenarioOutline.``0 Given scenario outline given step _Example Column 1_``
                        0
                        "Given"
                        "scenario outline given step <Example Column 1>"

            testCase
                "Scenario Outline Given multiline argument correct"
                <| fun _ ->

                    let step = scenarioOutline.``0 Given scenario outline given step _Example Column 1_``
                    let expectedArgument = "multi line\r\nscenario outline\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Scenario Outline Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Scenario Outline When correct"
                <| fun _ ->

                    validateStep
                        scenarioOutline.``1 When scenario outline when step _Example Column 2_``
                        1
                        "When"
                        "scenario outline when step <Example Column 2>"

            testCase
                "Scenario Outline When data table argument correct"
                <| fun _ ->

                    let step = scenarioOutline.``1 When scenario outline when step _Example Column 2_``
                    Expect.equal step.Argument.Length 2 (sprintf "Scenario Outline When Data:Expected 2 rows but got %i" step.Argument.Length)

                    validateData step.Argument.[0].column1 "column1" "data1"
                    validateData step.Argument.[0].column2 "column2" "data2"
                    validateData step.Argument.[1].column1 "column1" "data3"
                    validateData step.Argument.[1].column2 "column2" "data4"

            testCase
                "Scenario Outline Then correct"
                <| fun _ ->

                    validateStep
                        scenarioOutline.``2 Then scenario outline then step _Example Column 3_``
                        2
                        "Then"
                        "scenario outline then step <Example Column 3>"
        ]