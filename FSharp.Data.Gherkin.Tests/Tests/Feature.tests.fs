module FSharp.Data.Gherking.Tests.Feature

open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers
open Expecto


[<Tests>]
let features =

        let feature = TestFeature.``Feature name``
        testList
            "Feature correct"
            [
                testCase
                    "Feature details correct"
                    <| fun _ -> validateFeature feature "Feature name"  "Multi-line\r\nFeature Description"

                testCase
                    "Feature tags correct"
                    <| fun _ ->
                        let tags = feature.Tags |> Seq.toList
                        Expect.equal tags.[0] "@featureTag1" (sprintf "Expected tag name %s but got %s" "@featureTag1" tags.[0])
                        Expect.equal tags.[1] "@featureTag2" (sprintf "Expected tag name %s but got %s" "@featureTag2" tags.[1])
            ]