module FSharp.Data.Gherking.Tests.Scenario

open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers

open Expecto


[<Tests>]
let scenario1 =

    let scenario1 = TestFeature.``Feature name``.Scenarios.``Scenario 1 name``

    testList
        "Scenario1 has correct data"
        [
            testCase
                "Scenario is correct"
                <| fun _ -> validateScenario scenario1 "Scenario 1 name" "Multi-line\r\nScenario 1 Description"

            testCase
                "Scenario 1 Given correct"
                <| fun _ ->

                    validateStep
                        scenario1.``0. Given scenario 1 given step``
                        0
                        "Given"
                        "scenario 1 given step"

            testCase
                "Scenario 1 Given multiline argument correct"
                <| fun _ ->

                    let step = scenario1.``0. Given scenario 1 given step``
                    let expectedArgument = "multi line\r\nscenario 1\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Background Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Scenario 1 When correct"
                <| fun _ ->

                    validateStep
                        scenario1.``1. When scenario 1 when step``
                        1
                        "When"
                        "scenario 1 when step"

            testCase
                "Scenario 1 When data table argument correct"
                <| fun _ ->

                    let step = scenario1.``1. When scenario 1 when step``
                    let dataTableRows = (step.Data |> Seq.toList)
                    Expect.equal dataTableRows.Length 2 (sprintf "Background When Data:Expected 2 rows but got %i" dataTableRows.Length)

                    validateDatarow  dataTableRows.[0].column1 "column1" "data1"
                    validateDatarow  dataTableRows.[0].column2 "column2" "data2"
                    validateDatarow  dataTableRows.[1].column1 "column1" "data3"
                    validateDatarow  dataTableRows.[1].column2 "column2" "data4"

            testCase
                "Scenario 1 Then correct"
                <| fun _ ->

                    validateStep
                        scenario1.``2. Then scenario 1 then step``
                        2
                        "Then"
                        "scenario 1 then step"
        ]

[<Tests>]
let scenario2 =

    let scenario2 = TestFeature.``Feature name``.Scenarios.``Scenario 2 name``

    testList
        "Scenario2 has correct data"
        [
            testCase
                "Scenario is correct"
                <| fun _ -> validateScenario scenario2 "Scenario 2 name" "Multi-line\r\nScenario 2 Description"

            testCase
                "Scenario 2 Given correct"
                <| fun _ ->

                    validateStep
                        scenario2.``0. Given scenario 2 given step``
                        0
                        "Given"
                        "scenario 2 given step"

            testCase
                "Scenario 2 Given multiline argument correct"
                <| fun _ ->

                    let step = scenario2.``0. Given scenario 2 given step``
                    let expectedArgument = "multi line\r\nscenario 2\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Background Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Scenario 2 When correct"
                <| fun _ ->

                    validateStep
                        scenario2.``1. When scenario 2 when step``
                        1
                        "When"
                        "scenario 2 when step"

            testCase
                "Scenario 2 When data table argument correct"
                <| fun _ ->

                    let step = scenario2.``1. When scenario 2 when step``
                    let dataTableRows = (step.Data |> Seq.toList)
                    Expect.equal dataTableRows.Length 2 (sprintf "Background When Data:Expected 2 rows but got %i" dataTableRows.Length)

                    validateDatarow  dataTableRows.[0].column1 "column1" "data1"
                    validateDatarow  dataTableRows.[0].column2 "column2" "data2"
                    validateDatarow  dataTableRows.[1].column1 "column1" "data3"
                    validateDatarow  dataTableRows.[1].column2 "column2" "data4"

            testCase
                "Scenario 1 Then correct"
                <| fun _ ->

                    validateStep
                        scenario2.``2. Then scenario 2 then step``
                        2
                        "Then"
                        "scenario 2 then step"
        ]