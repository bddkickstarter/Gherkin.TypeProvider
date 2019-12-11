module FSharp.Data.Gherkin.Tests.Feature

open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers
open Expecto


[<Tests>]
let features =

        let feature = TestFeature.CreateFeature()
        testList
            "Feature correct"
            [
                testCase
                    "Feature details correct"
                    <| fun _ -> validateFeature feature "Feature name"  "Multi-line\r\nFeature Description"

                testCase
                    "Feature tags correct"
                    <| fun _ ->
                        let tag1 = feature.Tags.featureTag1.Name
                        let tag2 = feature.Tags.featureTag2.Name

                        Expect.equal tag1 "@featureTag1" (sprintf "Expected tag name %s but got %s" "@featureTag1" tag1)
                        Expect.equal tag2 "@featureTag2" (sprintf "Expected tag name %s but got %s" "@featureTag2" tag2)

                testCase
                    "Feature has correct number of scenarios"
                    <| fun _ -> Expect.isTrue (feature.Scenarios.Length = 3) (sprintf "Expected 3 scenarios but got %i" feature.Scenarios.Length)
            ]