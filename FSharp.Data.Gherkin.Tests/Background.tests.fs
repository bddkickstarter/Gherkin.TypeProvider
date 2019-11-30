module FSharp.Data.Gherking.Tests.Background

open FSharp.Data.Gherkin
open Expecto

// Use the const __SOURCE_DIRECTORY__ here to help intellisense play nice with relative paths
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

let feature = TestFeature.``Feature name``


[<Tests>]
let background =

    let background = feature.Background
    
    testList
        "Background has correct data"
        [
            testCase
                "Background name correct"
                <| fun _ ->
                    let expectedBackgroundName = "Background name"
                    Expect.equal background.Name expectedBackgroundName (sprintf "Background name:Expected %s but got %s" expectedBackgroundName background.Name)

            testCase
                "Background description correct"
                <| fun _ ->
                    let expectedBackgroundDescription = "Multi-line\r\nBackground Description"
                    Expect.equal background.Description expectedBackgroundDescription (sprintf "Background description:Expected %s but got %s" expectedBackgroundDescription background.Description)

            testCase
                "Background Given correct"
                <| fun _ ->
                    let actualGivenText =  background.``0. Given background given step``.StepName
                    let expectedGivenText = "background given step"
                    Expect.equal actualGivenText expectedGivenText  (sprintf "Background given:Expected %s but got %s" expectedGivenText actualGivenText)
        ]