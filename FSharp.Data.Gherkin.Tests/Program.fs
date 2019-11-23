// Learn more about F# at http://fsharp.org

open System
open Expecto

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"C:\\src\\bddkickstarter\\Gherkin.TypeProvider\\FSharp.Data.Gherkin.Tests\\test.feature">


[<Tests>]
let foo = 
    testCase
        "bar"
        <| fun _ ->
            let feature = TestFeature.``this is a feature``.Examples.[0]
            let example = "" //feature.Examples.[0]
            let scenario = TestFeature.``this is a feature``.Scenarios.``this is a scenario``.ScenarioName

            printfn "Feature:%A Scenario:%A" feature scenario
            //printfn "Background:%A Scenario:%A" "kjh" "lkj"
                
                
               

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv