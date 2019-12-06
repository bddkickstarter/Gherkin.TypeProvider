module FSharp.Data.Gherkin.Tests.Feature

open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers
open Expecto


[<Tests>]
let features =

        let feature = TestFeature.Feature
        testList
            "Feature correct"
            [
                testCase
                    "Feature details correct"
                    <| fun _ -> validateFeature feature "Feature name"  "Multi-line\r\nFeature Description"

                testCase
                    "Feature tags correct"
                    <| fun _ ->
                        let tag1 = feature.Tags.featureTag1
                        let tag2 = feature.Tags.featureTag2

                        Expect.equal tag1 "@featureTag1" (sprintf "Expected tag name %s but got %s" "@featureTag1" tag1)
                        Expect.equal tag2 "@featureTag2" (sprintf "Expected tag name %s but got %s" "@featureTag2" tag2)
            ]