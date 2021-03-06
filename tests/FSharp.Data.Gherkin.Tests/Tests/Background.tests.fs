module FSharp.Data.Gherkin.Tests.Background

open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers

open Expecto

[<Tests>]
let background =

    let background = TestFeature.CreateFeature().Background
        
    testList
        "Background has correct data"
        [
            testCase
                "Background correct"
                <| fun _ ->
                    validateBackground background "Background name" "Multi-line\r\nBackground Description"

            testCase
                "Background Given correct"
                <| fun _ ->

                    validateStep
                        background.``0 Given background given step`` 
                        0
                        "Given"
                        "background given step"

            testCase
                "Background Given multiline argument correct"
                <| fun _ ->

                    let step = background.``0 Given background given step``
                    let expectedArgument = "multi line\r\nbackground\r\nargument"
                    Expect.equal 
                        step.Argument.Content expectedArgument
                        (sprintf 
                            "Background Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Background When correct"
                <| fun _ ->

                    validateStep
                        background.``1 When background when step``
                        1
                        "When"
                        "background when step"

            testCase
                "Background When data table argument correct"
                <| fun _ ->

                    let step = background.``1 When background when step``
                    Expect.equal step.Argument.Length 2 (sprintf "Background When Data:Expected 2 rows but got %i" step.Argument.Length)

                    validateData step.Argument.[0].column1 "column1" "data1"
                    validateData step.Argument.[0].column2 "column2" "data2"
                    validateData step.Argument.[1].column1 "column1" "data3"
                    validateData step.Argument.[1].column2 "column2" "data4"

            testCase
                "Background Then correct"
                <| fun _ ->

                    validateStep
                        background.``2 Then background then step``
                        2
                        "Then"
                        "background then step"
        ]