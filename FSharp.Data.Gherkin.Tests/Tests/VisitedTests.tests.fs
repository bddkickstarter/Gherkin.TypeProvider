module FSharp.Data.Gherkin.Tests.Visited

open Expecto
open FSharp.Data.Gherkin.Tests.Model

[<Tests>]
let visitScenarios =
    testCase
        "Scenarios marked as visited if accessed through named property"
        <| fun _ -> 
            let feature = TestFeature.CreateFeature()

            Expect.isFalse feature.Scenarios.[0].Visited "Expected visit to scenario 1 to be false before visiting"
            Expect.isFalse feature.Scenarios.[1].Visited "Expected visit to scenario 2 to be false before visiting"
            Expect.isFalse feature.Scenarios.[2].Visited "Expected visit to scenario outline to be false before visiting"

            feature.``Scenario 1 name`` |> ignore
            feature.``Scenario 2 name`` |> ignore

            Expect.isTrue feature.Scenarios.[0].Visited "Expected visit to scenario 1 to be true after visiting"
            Expect.isTrue feature.Scenarios.[1].Visited "Expected visit to scenario 2 to be true before visiting"

[<Tests>]
let visitSteps =
    testCase
        "Steps marked as visited if accessed through named property"
        <| fun _ -> 
            let feature = TestFeature.CreateFeature()

            let scenario = feature.``Scenario 1 name``

            Expect.isFalse scenario.Steps.[0].Visited "Expected visit to given step to be false before visiting"
            Expect.isFalse scenario.Steps.[1].Visited "Expected visit to when to be false before visiting"
            Expect.isFalse scenario.Steps.[2].Visited "Expected visit to then to be false before visiting"

            scenario.``0 Given scenario 1 given step`` |> ignore
            scenario.``1 When scenario 1 when step`` |> ignore

            Expect.isTrue scenario.Steps.[0].Visited "Expected visit to given to be true after visiting"
            Expect.isTrue scenario.Steps.[1].Visited "Expected visit to when to be true before visiting"
            Expect.isFalse scenario.Steps.[2].Visited "Expected visit to then to be false as not visited"

