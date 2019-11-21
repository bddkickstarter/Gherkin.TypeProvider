// Learn more about F# at http://fsharp.org

open System
open Expecto

open ProviderImplementation

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"C:\\src\\bddkickstarter\\Gherkin.TypeProvider\\FSharp.Data.Gherkin.Tests\\test.feature">


[<Tests>]
let foo = 
    testCase
        "bar"
        <| fun _ ->
            let background = TestFeature.Background.``0. Given this is a background given``.StepText
            let scenario = TestFeature.Scenarios.``this is a scenario``.``0. Given this is a scenario given1``.StepText
            printfn "%A %A" background scenario
                
                
               

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv
