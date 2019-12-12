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


let walkTags (tags:TestFeature.TestFeature_TagBase[]) =
    match 
        tags
        |> Array.filter(fun t -> not t.Visited)
        |> Array.fold(fun a c -> sprintf "Tag%s not visited\r\n%s" c.Name a) "" with
    | "" -> Ok()
    | err -> Error err

let walkData (data:TestFeature.TestFeature_DataRowBase[]) =
    if isNull data then Ok()
    else
        match 
            data
            |> Array.mapi(fun i row -> 
                row.Cells 
                |> Array.filter (fun c -> not c.Visited)
                |> Array.fold(
                           fun a c -> 
                            sprintf "Row %i Column %s not visited\r\n%s" i c.Header a) "") 
            |> Array.fold(fun (a:string) c ->
                if a.Length = 0 then  sprintf "%s%s" a c
                else sprintf "%s%s\r\n" a c
                ) "" with
        | "" -> Ok()
        | err -> Error err

let walkSteps (steps:TestFeature.TestFeature_StepBase[]) =
    match
        steps
        |> Array.map(fun s ->
                if not s.Visited then sprintf "Step %i not visited" s.Order
                else if not (isNull s.DocString) && not s.DocString.Visited then sprintf "Step %i Doc String argument not visited" s.Order
                else
                    match walkData s.DataTable with
                    | Error err -> sprintf "Step:%i %s" s.Order err
                    | Ok() ->
                        if not s.Visited then sprintf "Step %i not visited" s.Order
                        else "")
        |> Array.fold (fun (a:string) c -> 
                if a.Length = 0 then  sprintf "%s%s" a c
                else sprintf "%s%s\r\n" a c
         ) "" with
    | "" -> Ok()
    | err -> Error err

let walkScenarios (scenarios:TestFeature.TestFeature_ScenarioBase[]) =
    match 
        scenarios
        |> Array.map(fun s ->
            if not s.Visited then sprintf "Scenario %s not visited" s.Name
            else 
                match walkSteps s.Steps with
                | Error err -> sprintf "%s\r\n%s" s.Name err
                | Ok() -> "")
        |> Array.fold (fun (a:string) c ->
                if a.Length = 0 then  sprintf "%s%s" a c
                else sprintf "%s%s\r\n" a c
         ) "" with
    | "" -> Ok()
    | err -> Error err

let walkFeature (feature:TestFeature.TestFeature_Feature) =
    match walkTags feature.Tags.AllTags with
    | Error err -> failwith err
    | Ok() ->

    walkScenarios feature.Scenarios
        

[<Tests>]
let walkTheFeature =
    testCase
        "Walk the feature using underlying arrays"
        <| fun _ ->
            let feature = TestFeature.CreateFeature()

            feature.Tags.featureTag1 |> ignore
            feature.Tags.featureTag2 |> ignore

            let scenario1 = feature.``Scenario 1 name``
            let scenario2 = feature.``Scenario 2 name``
            let scenarioOutline = feature.``Scenario outline name``

            scenario1.``0 Given scenario 1 given step``.Argument.Content |> ignore
            scenario1.``1 When scenario 1 when step``.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)
            scenario1.``2 Then scenario 1 then step`` |> ignore
            
            scenario2.``0 Given scenario 2 given step``.Argument.Content |> ignore
            scenario2.``1 When scenario 2 when step``.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)
            scenario2.``2 Then scenario 2 then step`` |> ignore

            let scenarioOutlineGiven = scenarioOutline.``0 Given scenario outline given step <Example Column 1>``
            let scenarioOutlineWhen = scenarioOutline.``1 When scenario outline when step <Example Column 2>``
            scenarioOutline.``2 Then scenario outline then step <Example Column 3>`` |> ignore

            scenarioOutlineGiven.Argument.Content |> ignore
            scenarioOutlineWhen.Argument |> Seq.iter(fun rw -> (rw.column1,rw.column2) |> ignore)
            
            match walkFeature feature with
            | Ok() -> ()
            | Error err -> failwith err
            

            

            

