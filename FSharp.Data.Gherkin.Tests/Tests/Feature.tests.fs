module FSharp.Data.Gherking.Tests.Feature

open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers
open Expecto


[<Tests>]
let features =
        testCase
            "Feature correct"
            <| fun _ -> validateFeature TestFeature.``Feature name`` "Feature name"  "Multi-line\r\nFeature Description"