// Learn more about F# at http://fsharp.org

open System
open Expecto

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"C:\\src\\bddkickstarter\\Gherkin.TypeProvider\\FSharp.Data.Gherkin.Tests\\test.feature">


[<Tests>]
let foo = 
    testCase
        "bar"
        <| fun _ ->
            let feature = TestFeature.``this is a feature``.FeatureName
            let scenario = TestFeature.``this is a feature``.Scenarios.``this is a scenario``.ScenarioName
            let scenarioOutline = TestFeature.``this is a feature``.ScenarioOutlines.``this is a scenario outline``.ScenarioName
            let example = TestFeature.``this is a feature``.ScenarioOutlines.``this is a scenario outline``.Examples.[0]
            printfn "Feature:%A Scenario:%A Scenario Outline:%A Example:%A" feature scenario scenarioOutline example
            //printfn "Background:%A Scenario:%A" "kjh" "lkj"
                
                
               

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv