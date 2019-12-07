module FSharp.Data.Gherkin.Tests.Scenario

open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers

open Expecto

[<Tests>]
let scenario1 =

    let scenario1 = TestFeature.CreateFeature().``Scenario 1 name``

    testList
        "Scenario 1 has correct data"
        [
            testCase
                "Scenario 1 is correct"
                <| fun _ -> validateScenario scenario1 "Scenario 1 name" "Multi-line\r\nScenario 1 Description"

            testCase
                    "Scenario 1 tags correct"
                    <| fun _ ->
                        let tag1 = scenario1.Tags.scenario1Tag1
                        let tag2 = scenario1.Tags.scenario1Tag2

                        Expect.equal tag1 "@scenario1Tag1" (sprintf "Expected tag name %s but got %s" "@scenario1Tag1" tag1)
                        Expect.equal tag2 "@scenario1Tag2" (sprintf "Expected tag name %s but got %s" "@scenario1Tag2" tag2)

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
                        step.DocString.Content expectedArgument
                        (sprintf 
                            "Scenario 1 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.DocString.Content)

            testCase
                "Scenario 1 When correct"
                <| fun _ ->

                    validateStep
                        scenario1.``1 When scenario 1 when step``
                        1
                        "When"
                        "scenario 1 when step"

            // testCase
            //     "Scenario 1 When data table argument correct"
            //     <| fun _ ->

            //         let step = scenario1.``1 When scenario 1 when step``
            //         let dataTableRows = (step.Data |> Seq.toList)
            //         Expect.equal dataTableRows.Length 2 (sprintf "Scenario 1 When Data:Expected 2 rows but got %i" dataTableRows.Length)

            //         validateData dataTableRows.[0].column1 "column1" "data1"
            //         validateData dataTableRows.[0].column2 "column2" "data2"
            //         validateData dataTableRows.[1].column1 "column1" "data3"
            //         validateData dataTableRows.[1].column2 "column2" "data4"

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

    let scenario2 = TestFeature.CreateFeature().``Scenario 2 name``

    testList
        "Scenario 2 has correct data"
        [
            // testCase
            //     "Scenario 2 is correct"
            //     <| fun _ -> validateScenario scenario2 "Scenario 2 name" "Multi-line\r\nScenario 2 Description"

            testCase
                    "Scenario 2 tags correct"
                    <| fun _ ->
                        let tag1 = scenario2.Tags.scenario2Tag1
                        let tag2 = scenario2.Tags.scenario2Tag2

                        Expect.equal tag1 "@scenario2Tag1" (sprintf "Expected tag name %s but got %s" "@scenario2Tag1" tag1)
                        Expect.equal tag2 "@scenario2Tag2" (sprintf "Expected tag name %s but got %s" "@scenario2Tag2" tag2)

            // testCase
            //     "Scenario 2 Given correct"
            //     <| fun _ ->

            //         validateStep
            //             scenario2.``0 Given scenario 2 given step``
            //             0
            //             "Given"
            //             "scenario 2 given step"

            testCase
                "Scenario 2 Given multiline argument correct"
                <| fun _ ->

                    let step = scenario2.``0 Given scenario 2 given step``
                    let expectedArgument = "multi line\r\nscenario 2\r\nargument"
                    Expect.equal 
                        step.DocString.Content expectedArgument
                        (sprintf 
                            "Scenario 2 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.DocString.Content)

            // testCase
            //     "Scenario 2 When correct"
            //     <| fun _ ->

            //         validateStep
            //             scenario2.``1 When scenario 2 when step``
            //             1
            //             "When"
            //             "scenario 2 when step"

            // testCase
            //     "Scenario 2 When data table argument correct"
            //     <| fun _ ->

            //         let step = scenario2.``1. When scenario 2 when step``
            //         let dataTableRows = (step.Data |> Seq.toList)
            //         Expect.equal dataTableRows.Length 2 (sprintf "Scenario 2 When Data:Expected 2 rows but got %i" dataTableRows.Length)

            //         validateData dataTableRows.[0].column1 "column1" "data1"
            //         validateData dataTableRows.[0].column2 "column2" "data2"
            //         validateData dataTableRows.[1].column1 "column1" "data3"
            //         validateData dataTableRows.[1].column2 "column2" "data4"

            // testCase
            //     "Scenario 1 Then correct"
            //     <| fun _ ->

            //         validateStep
            //             scenario2.``2 Then scenario 2 then step``
            //             2
            //             "Then"
            //             "scenario 2 then step"
        ]