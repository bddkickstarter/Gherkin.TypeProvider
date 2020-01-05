module FSharp.Data.Gherkin.Tests.Scenario

open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers

open Expecto

[<Tests>]
let scenario1 =

    let scenario1 = TestFeature.CreateFeature().Scenarios.``Scenario 1 name``

    testList
        "Scenario 1 has correct data"
        [
            testCase
                "Scenario has visiting tags correct"
                <| fun _ ->
                    let scenario = TestFeature.CreateFeature().Scenarios.``Scenario 1 name``
                    let before = scenario.TagList.AllTags.[0].Visited

                    Expect.isFalse before "Expected tag to be unvisited"

                    let hasTag = scenario.HasTag("@scenario1Tag1")

                    Expect.isTrue hasTag "Expected tag to be present"

                    let after = scenario.TagList.AllTags.[0].Visited

                    Expect.isTrue after "Expected tag to be unvisited"

            testCase
                "Scenario hastags false if no tag"
                <| fun _ ->
                    let scenario = TestFeature.CreateFeature().Scenarios.``Scenario 1 name``
                    
                    let hasTag = scenario.HasTag("@foo")

                    Expect.isFalse hasTag "Expected tag to be missing"

            testCase
                "Scenario 1 is correct"
                <| fun _ -> validateScenario scenario1 "Scenario 1 name" "Multi-line\r\nScenario 1 Description"

            testCase
                    "Scenario 1 tags correct"
                    <| fun _ ->
                        let tag1 = scenario1.Tags.scenario1Tag1.Name
                        let tag2 = scenario1.Tags.scenario1Tag2.Name

                        Expect.equal tag1 "@scenario1Tag1" (sprintf "Expected tag name %s but got %s" "@scenario1Tag1" tag1)
                        Expect.equal tag2 "@scenario1Tag2" (sprintf "Expected tag name %s but got %s" "@scenario1Tag2" tag2)


            testCase
                "Scenario 1 has correct number of steps"
                <| fun _ -> Expect.isTrue (scenario1.Steps.Length = 3) (sprintf "Expected 3 steps but got %i" scenario1.Steps.Length)

            testCase
                "Scenario 1 Given correct"
                <| fun _ ->

                    validateStep
                        scenario1.``0 Given scenario 1 given step``
                        0
                        "Given"
                        "scenario 1 given step"

            testCase
                "Scenario 1 Given multiline argument correct"
                <| fun _ ->

                    let step = scenario1.``0 Given scenario 1 given step``
                    let expectedArgument = "multi line\r\nscenario 1\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Scenario 1 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Scenario 1 When correct"
                <| fun _ ->

                    validateStep
                        scenario1.``1 When scenario 1 when step``
                        1
                        "When"
                        "scenario 1 when step"

            testCase
                "Scenario 1 When data table argument correct"
                <| fun _ ->

                    let step = scenario1.``1 When scenario 1 when step``
                    Expect.equal step.Argument.Length 2 (sprintf "Scenario 1 When Data:Expected 2 rows but got %i" step.Argument.Length)

                    validateData step.Argument.[0].column1 "column1" "data1"
                    validateData step.Argument.[0].column2 "column2" "data2"
                    validateData step.Argument.[1].column1 "column1" "data3"
                    validateData step.Argument.[1].column2 "column2" "data4"

            testCase
                "Scenario 1 Then correct"
                <| fun _ ->

                    validateStep
                        scenario1.``2 Then scenario 1 then step``
                        2
                        "Then"
                        "scenario 1 then step"
        ]

[<Tests>]
let scenario2 =

    let scenario2 = TestFeature.CreateFeature().Scenarios.``Scenario 2 name``

    testList
        "Scenario 2 has correct data"
        [
            testCase
                "Scenario 2 is correct"
                <| fun _ -> validateScenario scenario2 "Scenario 2 name" "Multi-line\r\nScenario 2 Description"

            testCase
                    "Scenario 2 tags correct"
                    <| fun _ ->
                        let tag1 = scenario2.Tags.scenario2Tag1.Name
                        let tag2 = scenario2.Tags.scenario2Tag2.Name

                        Expect.equal tag1 "@scenario2Tag1" (sprintf "Expected tag name %s but got %s" "@scenario2Tag1" tag1)
                        Expect.equal tag2 "@scenario2Tag2" (sprintf "Expected tag name %s but got %s" "@scenario2Tag2" tag2)

            testCase
                "Scenario 2 has correct number of steps"
                <| fun _ -> Expect.isTrue (scenario2.Steps.Length = 3) (sprintf "Expected 3 steps but got %i" scenario2.Steps.Length)

            testCase
                "Scenario 2 Given correct"
                <| fun _ ->

                    validateStep
                        scenario2.``0 Given scenario 2 given step``
                        0
                        "Given"
                        "scenario 2 given step"

            testCase
                "Scenario 2 Given multiline argument correct"
                <| fun _ ->

                    let step = scenario2.``0 Given scenario 2 given step``
                    let expectedArgument = "multi line\r\nscenario 2\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Scenario 2 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Scenario 2 When correct"
                <| fun _ ->

                    validateStep
                        scenario2.``1 When scenario 2 when step``
                        1
                        "When"
                        "scenario 2 when step"

            testCase
                "Scenario 2 When data table argument correct"
                <| fun _ ->

                    let step = scenario2.``1 When scenario 2 when step``
                    Expect.equal step.Argument.Length 2 (sprintf "Scenario 2 When Data:Expected 2 rows but got %i" step.Argument.Length)

                    validateData step.Argument.[0].column1 "column1" "data1"
                    validateData step.Argument.[0].column2 "column2" "data2"
                    validateData step.Argument.[1].column1 "column1" "data3"
                    validateData step.Argument.[1].column2 "column2" "data4"

            testCase
                "Scenario 1 Then correct"
                <| fun _ ->

                    validateStep
                        scenario2.``2 Then scenario 2 then step``
                        2
                        "Then"
                        "scenario 2 then step"
        ]