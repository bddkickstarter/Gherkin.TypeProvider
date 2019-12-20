module FSharp.Data.Gherkin

open GherkinProvider.Validation
open GherkinProvider.Builders

let inline (>>=) (nm,lst) tl = tl nm lst

let inline scenarioOutline outline = ScenarioOutline(outline)

let inline validateFeature feature = FeatureValidator.Validate feature

let inline validateFeatureRun feature args result =
        if result <> 0 then result
        else
            match validateFeature (feature,args) with
            | None -> 0
            | Some report -> failwithf "Validation Failure:\r\n%s" report.Summary

let args = [""]

let foo args  = 0
let feauture = obj()

let i = foo args |> validateFeatureRun feauture args