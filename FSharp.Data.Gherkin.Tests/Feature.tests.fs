module FSharp.Data.Gherking.Tests.Feature

open FSharp.Data.Gherkin
open Expecto

// Use the const __SOURCE_DIRECTORY__ here to help intellisense play nice with relative paths
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

let feature = TestFeature.``Feature name``

[<Tests>]
let features =
    testList
        "Feature has correct data"
        [
            testCase
                "Feature name correct"
                <| fun _ ->
                    let expectedFeatureName = "Feature name"
                    Expect.equal feature.FeatureName expectedFeatureName (sprintf "Feature name:Expected %s but got %s" expectedFeatureName feature.FeatureName)

            testCase
                "Feature description correct"
                <| fun _ ->
                    let expectedFeatureDescription = "Multi-line\r\nFeature Description"
                    Expect.equal feature.FeatureDescription expectedFeatureDescription (sprintf "Feature description:Expected %s but got %s" expectedFeatureDescription feature.FeatureDescription)
        ]