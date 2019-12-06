open Expecto
open Gherkin
open FSharp.Data.Gherkin

//type FooFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

 [<EntryPoint>]
 let main argv =
    runTestsInAssembly defaultConfig argv