// Learn more about F# at http://fsharp.org

open System
open Expecto

open ProviderImplementation

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"C:\\src\\bddkickstarter\\FSharp.Data.Gherkin\\FSharp.Data.Gherkin.Tests\\test.feature">


[<Tests>]
let foo = 
    testCase
        "bar"
        <| fun _ ->
            let test = TestFeature.``this is another scenario``.``0. Given this is another scenario given1``
            printfn "%A" test
                
                
               

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv
