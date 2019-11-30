module FSharp.Data.Gherkin.Tests.Background

open FSharp.Data.Gherkin
open FSharp.Data.Gherkin.Tests.Features

open Expecto

let validateBackground (background:Background) (name:string) (description:string) =
    Expect.equal background.Name name (sprintf "Background name:Expected %s but got %s" name background.Name)
    Expect.equal background.Description description (sprintf "Background description:Expected %s but got %s" description background.Description)

let validateStep (step:Step) (expectedOrder:int) (expectedKeyword:string) (expectedText:string) =
    Expect.equal step.Order expectedOrder (sprintf "Step Order:Expected %i but got %i" expectedOrder step.Order)
    Expect.equal step.Keyword expectedKeyword (sprintf "Step Keyword:Expected %s but got %s" expectedKeyword step.Keyword)
    Expect.equal step.Text expectedText (sprintf "Step Text:Expected %s but got %s" expectedText step.Text)

let validateDatarow (row:DataCell) (header:string) (value:string) =
    Expect.equal row.Header header (sprintf "Background When Data Row 1 Header:Expected %s but got %s" header row.Header)
    Expect.equal row.Value value (sprintf "Background When Data Row 1 Value:Expected %s but got %s" value row.Value)


[<Tests>]
let background =

    let background = TestFeature.``Feature name``.Background
        
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
                        background.``0. Given background given step`` 
                        0
                        "Given"
                        "background given step"


            testCase
                "Background Given multiline argument correct"
                <| fun _ ->

                    let step = background.``0. Given background given step``
                    let expectedArgument = "multi line\r\nargument"
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
                        background.``1. When background when step``
                        1
                        "When"
                        "background when step"

            testCase
                "Background When data table argument correct"
                <| fun _ ->

                    let step = background.``1. When background when step``
                    let dataTableRows = (step.Data |> Seq.toList)
                    Expect.equal dataTableRows.Length 2 (sprintf "Background When Data:Expected 2 rows but got %i" dataTableRows.Length)

                    validateDatarow  dataTableRows.[0].column1 "column1" "data1"
                    validateDatarow  dataTableRows.[0].column2 "column2" "data2"
                    validateDatarow  dataTableRows.[1].column1 "column1" "data3"
                    validateDatarow  dataTableRows.[1].column2 "column2" "data4"

            testCase
                "Background Then correct"
                <| fun _ ->

                    validateStep
                        background.``2. Then background then step``
                        2
                        "Then"
                        "background then step"
        ]