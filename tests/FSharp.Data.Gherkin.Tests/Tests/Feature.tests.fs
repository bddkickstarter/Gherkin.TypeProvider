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
                    "Feature has tags correct"
                    <| fun _ ->
                        let feature = TestFeature.CreateFeature()
                        let before = feature.Tags.AllTags.[0].Visited

                        Expect.isFalse before "Expected tag to be unvisited"

                        let hasTag = feature.HasTag("@featureTag1")

                        Expect.isTrue hasTag "Expected tag to be present"

                        let after = feature.Tags.AllTags.[0].Visited

                        Expect.isTrue after "Expected tag to be unvisited"

                testCase
                    "Feature hastags false if no tag"
                    <| fun _ ->
                        let feature = TestFeature.CreateFeature()
                        
                        let hasTag = feature.HasTag("@foo")

                        Expect.isFalse hasTag "Expected tag to be missing"

                testCase
                    "Feature tags correct"
                    <| fun _ ->
                        let tag1 = feature.Tags.featureTag1.Name
                        let tag2 = feature.Tags.featureTag2.Name

                        Expect.equal tag1 "@featureTag1" (sprintf "Expected tag name %s but got %s" "@featureTag1" tag1)
                        Expect.equal tag2 "@featureTag2" (sprintf "Expected tag name %s but got %s" "@featureTag2" tag2)

                testCase
                    "Feature has correct number of scenarios"
                    <| fun _ -> Expect.isTrue (feature.Scenarios.All.Length = 4) (sprintf "Expected 4 scenarios but got %i" feature.Scenarios.All.Length)
            ]