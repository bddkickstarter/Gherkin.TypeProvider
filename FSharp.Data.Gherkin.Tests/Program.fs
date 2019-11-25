// Learn more about F# at http://fsharp.org

open System
open Expecto

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"./test.feature">
let feature = TestFeature.``this is a feature``

// [<Tests>]
// let exploratory =
//     testCase
//         "test some of the things"
//         <| fun _ ->
            
//             let feature = TestFeature.``this is a feature``.FeatureName

//             let scenario = TestFeature.``this is a feature``.Scenarios.``this is a scenario``.ScenarioName
//             let step = TestFeature.``this is a feature``.Scenarios.``this is a scenario``.``0. Given this is a scenario given1``.StepName
//             let scenarioOutline =TestFeature.``this is a feature``.ScenarioOutlines.``this is a scenario outline``.ScenarioName
//             let example = (TestFeature.``this is a feature``.ScenarioOutlines.``this is another scenario outline``.Examples |> Seq.toList).[0].``col umn``.Value
//             let arg = (TestFeature.``this is a feature``.Scenarios.``this is a scenario``.``0. Given this is a scenario given1``.Argument |> Seq.toList).[0].``Data``.Value
//             let arg2 = TestFeature.``this is a feature``.Scenarios.``this is a scenario``.``2. When this is a scenario when1``.Argument.Content

//             let example2 = (TestFeature.``this is a feature``.ScenarioOutlines.``this is a scenario outline``.Examples |> Seq.toList).[0].qqqq.Value
            
//             printfn "Feature:%A Scenario:%A Scenario Outline:%A Step:%A Example:%A DataArg:%A StringArg:%A" feature scenario scenarioOutline step example2 arg arg2


[<Tests>]
let scenarios =
    
    let scenarios = feature.Scenarios

    testList
        feature.FeatureName
        [
            let scenario = feature.Scenarios.``a new scenario`` 
            testCase
                scenario.ScenarioName
                <| fun _ ->
                    printfn
                        "%s\r\n%s\r\n%s" 
                        scenario.``0. Given this is a brand new step``.StepText
                        scenario.``1. When the user is emailed``.StepText
                        scenario.``2. Then something``.StepText

            let scenario = scenarios.``this is a scenario``
            testCase
                scenario.ScenarioName
                <| fun _ ->
                    printfn
                        "%s\r\n%s\r\n%s" 
                        scenario.``0. Given this is a scenario given1``.StepText
                        scenario.``1. And this is a scenario given2``.StepText
                        scenario.``2. When this is a scenario when1``.StepText

                    
        ]


[<Tests>]
let scenarioOutlines =
    
    let scenarios = feature.ScenarioOutlines

    testList
        feature.FeatureName
        [
            testCase
                scenarios.``this is a scenario outline``.ScenarioName
                <| fun _ ->
                    let scenario =scenarios.``this is a scenario outline``

                    printfn
                        "%s\r\n%s\r\n%s" 
                        scenario.``0. When foo <ghj>``.StepText
                        scenario.``1. Then bar``.StepText
                        (scenario.Examples |> Seq.toList).[0].qqqq.Value
        ]
                
               
              

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv