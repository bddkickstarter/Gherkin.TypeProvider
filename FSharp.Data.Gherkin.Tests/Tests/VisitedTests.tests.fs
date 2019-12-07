module FSharp.Data.Gherkin.Tests.Visited

open Expecto
open FSharp.Data.Gherkin.Tests.Model

[<Tests>]
let visitScenarios =
    testCase
        "Scenarios marked as visited if accessed through named property"
        <| fun _ -> ()