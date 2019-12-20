module FSharp.Data.Gherkin

open GherkinProvider.Validation
open GherkinProvider.Builders

let inline (>>=) (nm,lst) tl = tl nm lst

let inline scenarioOutline outline = ScenarioOutline(outline)

let validateFeature feature  = 
    FeatureValidator.Validate(feature)

let validateFeatureAndExclude feature (args:string [])  = 
    FeatureValidator.Validate(feature,args |> Seq.toList)

let inline validateFeatureRun feature (args:string[]) result =
        if result <> 0 then result
        else
            match FeatureValidator.Validate(feature,(args |> Seq.toList)) with
            | None -> 0
            | Some report -> failwithf "Validation Failure:\r\n%s" report.Summary