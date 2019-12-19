module FSharp.Data.Gherkin.Tests.Visited

open FSharp.Data.Gherkin.Tests
open Expecto

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
            Expect.isFalse feature.Scenarios.[2].Visited "Expected visit to scenario outline to be false as not visited"

            feature.``Scenario outline name`` |> ignore

            Expect.isTrue feature.Scenarios.[2].Visited "Expected visit to scenario outline to be true as now visited"

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

[<Tests>]
let visitDocStringArgument =
    testCase
        "DocString arguments marked visited if accessed through Argument property"
        <| fun _ ->
            let stepWithDocstringArg = TestFeature.CreateFeature().``Scenario 1 name``.``0 Given scenario 1 given step``

            Expect.isFalse stepWithDocstringArg.DocString.Visited "Expected visit to step argument to be false before visiting"

            stepWithDocstringArg.Argument |> ignore

            Expect.isTrue stepWithDocstringArg.DocString.Visited "Expected visit to step argument to be true after visiting"

[<Tests>]
let visitTags =
    testList
        "Tags are marked as visited when accessed through their named property"
        [
            testCase
                "Feature tags are marked as visted when accessed through their named property"
                <| fun _ ->
                    let feature = TestFeature.CreateFeature()

                    Expect.isFalse feature.Tags.AllTags.[0].Visited "Expected visit to feature tag to be false before visiting"

                    feature.Tags.featureTag1 |> ignore

                    Expect.isTrue feature.Tags.AllTags.[0].Visited "Expected visit to feature tag to be true after visiting"

            testCase
                "Scenario tags are marked as visted when accessed through their named property"
                <| fun _ ->
                    let scenario = TestFeature.CreateFeature().``Scenario 1 name``

                    Expect.isFalse scenario.Tags.AllTags.[0].Visited "Expected visit to scenario tag to be false before visiting"

                    scenario.Tags.scenario1Tag1 |> ignore

                    Expect.isTrue scenario.Tags.AllTags.[0].Visited "Expected visit to scenario tag to be true after visiting"
        ]

[<Tests>]
let visitDataTableCell =
    testCase
        "Data table argument cells are marked as visited when accessed through their named property"        
        <| fun _ ->
            let step = TestFeature.CreateFeature().Background.``1 When background when step``

            Expect.isFalse step.DataTable.[0].Cells.[0].Visited "Expected visit to data table argument cell to be false before visiting"

            step.Argument.[0].column1 |> ignore

            Expect.isTrue step.DataTable.[0].Cells.[0].Visited "Expected visit to data table argument cell to be true after visiting"


[<Tests>]
let visitExampleCell =
    testCase
        "Example cells are marked as visited when accessed through their named property"        
        <| fun _ ->
            let example = TestFeature.CreateFeature().``Scenario outline name``.Examples.[0]

            Expect.isFalse example.Cells.[0].Visited "Expected visit to example cell to be false before visiting"

            example.``Example Column 1`` |> ignore

            Expect.isTrue example.Cells.[0].Visited "Expected visit to example cell to be true after visiting"

open FSharp.Data.Gherkin.Validation

[<Tests>]
let validateTheFeature =
    testList 
        "Validate features & scenarios"
        [
            testCase
                "Validate the complex feature using the feature validator"
                <| fun _ ->
                    let feature = TestFeature.CreateFeature()

                    feature.Tags.featureTag1 |> ignore
                    feature.Tags.featureTag2 |> ignore

                    let scenario1 = feature.``Scenario 1 name``
                    let scenario2 = feature.``Scenario 2 name``
                    let scenarioOutline = feature.``Scenario outline name``

                    scenario1.Tags.scenario1Tag1 |> ignore
                    scenario1.Tags.scenario1Tag2 |> ignore
                    scenario1.``0 Given scenario 1 given step``.Argument.Content |> ignore
                    scenario1.``1 When scenario 1 when step``.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)
                    scenario1.``2 Then scenario 1 then step`` |> ignore
                    
                    scenario2.Tags.scenario2Tag1 |> ignore
                    scenario2.Tags.scenario2Tag2 |> ignore
                    scenario2.``0 Given scenario 2 given step``.Argument.Content |> ignore
                    scenario2.``1 When scenario 2 when step``.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)
                    scenario2.``2 Then scenario 2 then step`` |> ignore

                    scenarioOutline.Tags.scenarioOutlineTag1 |> ignore
                    scenarioOutline.Tags.scenarioOutlineTag2 |> ignore
                    let scenarioOutlineGiven = scenarioOutline.``0 Given scenario outline given step _Example Column 1_``
                    let scenarioOutlineWhen = scenarioOutline.``1 When scenario outline when step _Example Column 2_``
                    scenarioOutline.``2 Then scenario outline then step _Example Column 3_`` |> ignore

                    scenarioOutlineGiven.Argument.Content |> ignore
                    scenarioOutlineWhen.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)

                    scenarioOutline.Examples |> Seq.iter(fun e ->
                       (e.``Example Column 1`` |> ignore,
                        e.``Example Column 2`` |> ignore,
                        e.``Example Column 3`` |> ignore) |> ignore)

                    feature.Simple.``0 Given just a given`` |> ignore
                    
                    match FeatureValidator.Validate feature with
                    | None -> ()
                    | Some report -> failwith(report.Summary)

            testCase 
               "Validate the simple feature using the feature validator"
               <| fun _ ->
                    let feature = SimpleFeature.CreateFeature()

                    match FeatureValidator.Validate feature with
                    | None -> failwith "Expected simple feature to fail validation"
                    | _-> ()

                    feature.``simple scenario``.``0 Given simple given`` |> ignore

                    match FeatureValidator.Validate feature with
                    | None -> ()
                    | Some report -> failwithf "Expected simple feature to pass validation\r\n%s" report.Summary
        ]


[<Tests>]
let ignoreByTag=
    testList 
        "ignore by tag"
        [
            testCase
                "Exclude Feature"
                <| fun _ ->
                    let feature = TestFeature.CreateFeature()

                    match FeatureValidator.Validate feature with
                    | None -> failwith "Expected feature to fail validation"
                    | _->
                        match FeatureValidator.Validate(feature,["@featureTag1"]) with
                        | Some _ -> failwith "Expected feature to be excluded"
                        | _ -> 
                             match FeatureValidator.Validate(feature,["@featureTag2"]) with
                             | Some _ ->  failwith "Expected feature to be excluded"
                             | None -> ()

            testCase
                "Exclude scenarios"
                <| fun _ ->
                
                    let feature = TestFeature.CreateFeature()
                    feature.Tags.featureTag1 |> ignore
                    feature.Tags.featureTag2 |> ignore
                    feature.Simple.``0 Given just a given`` |> ignore

                    match FeatureValidator.Validate feature with
                    | None -> failwith "Expected feature to fail validation"
                    | _ ->
                        match FeatureValidator.Validate(feature,["@scenario1Tag1";"@scenario2Tag1";"@scenarioOutlineTag1"]) with
                        | Some _ -> failwith "Expected scenario to be excluded"
                        | _ -> 
                             match FeatureValidator.Validate(feature,["@scenario1Tag2";"@scenario2Tag2";"@scenarioOutlineTag2"]) with
                             | Some _ ->  failwith "Expected scenario to be excluded"
                             | None -> ()
        ]

            

            

            

