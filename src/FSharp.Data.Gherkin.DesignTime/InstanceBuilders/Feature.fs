module InstanceBuilders.Feature

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast
open ObjectModel


let buildRows (context:GeneratedTypeContext) (columns:string list) (rowType:ProvidedTypeDefinition) (dataRow:TableRow list) =
        dataRow 
        |> List.map(
            fun dr -> 
                let parameters =
                    List.map2 (
                        fun (column:string) (cell:TableCell) -> 
                            Expr.NewObject(
                                context.DataCellType.GetConstructors().[0],
                                [Expr.Value(column);Expr.Value(cell.Value)])) columns (dr.Cells |> Seq.toList) 

                Expr.NewObject(rowType.GetConstructors().[0],parameters))

let buildExamples (context:GeneratedTypeContext) (examples:Examples list) (exampleType:ProvidedTypeDefinition) =
    match examples with
    | [] -> Expr.NewArray(exampleType,[])
    | examplesList ->
        let headers = examplesList.[0].TableHeader.Cells |> Seq.toList |> List.map(fun th -> th.Value)
        let rows=
            examplesList 
            |> List.collect(fun e -> e.TableBody |> Seq.toList)
            |> buildRows context headers exampleType

        Expr.NewArray(exampleType,rows)

let buildArgument (context:GeneratedTypeContext) (argument:Gherkin.Ast.StepArgument) (stepType:StepExpression) =
    match argument,stepType.Argument with
    | :? DocString,Some argTypeExpression -> 
        match argTypeExpression with
        | DocStringType docStringType ->
            let docString = argument :?> DocString
            let content = Expr.Value(docString.Content)
            let contentType = Expr.Value(docString.ContentType)
            Some (Expr.NewObject(docStringType.GetConstructors().[0],[content;contentType]))
        | _ -> None
    | :? DataTable,Some argTypeExpression -> 
        match argTypeExpression with
        | DataTableType (dataTableType) ->
            let dataTable = (argument :?> DataTable).Rows |> Seq.toList
            let headers = dataTable.Head.Cells |> Seq.toList |> List.map(fun th -> th.Value)
            let rows = dataTable.Tail
            let dataTableRows = buildRows context headers dataTableType rows
            
            Some (Expr.NewArray(dataTableType,dataTableRows))
        | _ -> None
    | _ -> None

let buildSteps (context:GeneratedTypeContext) (steps:Step list) (stepsType:StepExpression list) =
    List.mapi2(
        fun i (step:Step) (stepType:StepExpression) ->
            let parameters = 
                let text = step.Text.Trim()
                let keyword = step.Keyword.Trim()
                match buildArgument context step.Argument stepType with
                | None -> 
                    [
                        Expr.Value(text)
                        Expr.Value(keyword)
                        Expr.Value(i)
                        Expr.Coerce(Expr.Value(null),context.ArgumentBaseType)
                    ]   
                | Some arg -> 
                    [
                        Expr.Value(text)
                        Expr.Value(keyword)
                        Expr.Value(i)
                        arg
                    ]  
            Expr.NewObject(stepType.Type.GetConstructors().[0],parameters)
    ) steps stepsType

let createTagInstance (tagType:ProvidedTypeDefinition) (tags:Tag list) =
    Expr.NewObject(tagType.GetConstructors().[0],(tags |> List.map(fun t -> Expr.Value(t.Name))))

let buildScenarios (context:GeneratedTypeContext) (scenarios:Scenario list) (scenarioTypes:ScenarioExpression list) =

    List.map2(
        fun (scenario:Scenario) (scenarioType:ScenarioExpression) ->
            let name = Expr.Value(scenario.Name)
            let description = Expr.Value(scenario.Description)
            let steps = buildSteps context (scenario.Steps |> Seq.toList) scenarioType.Steps

            let parameters = 
                match scenarioType.Examples,(scenario.Tags |> Seq.toList)  with
                | None,[] -> 
                    name :: description :: steps

                | Some exampleType,[] ->
                    let examples = buildExamples context (scenario.Examples |> Seq.toList) exampleType
                    name :: description :: examples :: steps

                | None,tags ->
                    match scenarioType.Tags with
                    | None -> name :: description :: steps
                    | Some tagType ->
                        let tagsInstance = createTagInstance tagType tags
                        name :: description :: tagsInstance :: steps

                | Some exampleType,tags ->

                    let examples = buildExamples context (scenario.Examples |> Seq.toList) exampleType

                    match scenarioType.Tags with
                    | None -> name :: description :: examples :: steps
                    | Some tagType ->
                        let tagsInstance = createTagInstance tagType tags
                        name :: description :: examples :: tagsInstance :: steps

            Expr.NewObject(scenarioType.Type.GetConstructors().[0],parameters)
    ) scenarios scenarioTypes

let buildBackground (backgroundType:ProvidedTypeDefinition) (gherkinBackground:Background) (steps:Expr list)=
    let nameParam = Expr.Value(gherkinBackground.Name)
    let descriptionParam = Expr.Value(gherkinBackground.Description)
    let parameters = nameParam :: descriptionParam :: steps

    Expr.NewObject(backgroundType.GetConstructors().[0],parameters)

let buildFeatureInstance (context:GeneratedTypeContext) (root:ProvidedTypeDefinition) (gherkinDocument:GherkinDocument) (featureExpression:FeatureExpression) =

    let gherkinScenarios =
        gherkinDocument.Feature.Children |> Seq.toList
        |> List.choose(
            fun c ->
                match c with
                | :? Scenario -> Some  (c :?> Scenario)
                | _ -> None
            )

    let gherkinBackground = 
        gherkinDocument.Feature.Children 
            |> Seq.tryPick(
                fun c ->
                    match c with
                    | :? Background -> Some  (c :?> Background)
                    | _ -> None
                )

    let parameters = 
         match featureExpression.Background,gherkinBackground,(gherkinDocument.Feature.Tags |> Seq.toList) with

         | Some bgType,Some gherkinBackground,[] ->

            let steps =  buildSteps context (gherkinBackground.Steps |> Seq.toList) bgType.Steps
            let background = buildBackground bgType.Type gherkinBackground steps
            
            [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background]

         | Some bgType,Some gherkinBackground,tags ->
            
            let steps =  buildSteps context (gherkinBackground.Steps |> Seq.toList) bgType.Steps
            let background = buildBackground bgType.Type gherkinBackground steps

            match featureExpression.Tags with
            | None ->  
                [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background]
            | Some featureTagType -> 
                let tagsInstance = createTagInstance featureTagType tags
                [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background;tagsInstance]

         | None, _,tags ->
            
            match featureExpression.Tags with
            | Some featureTagType -> 
                let tagsinstance = createTagInstance featureTagType tags
                [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);tagsinstance]
            | None -> [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description)]

         | _ -> [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description)]

    let scenarios = buildScenarios context gherkinScenarios featureExpression.Scenarios

    let allParams = parameters @ scenarios

    let feature = Expr.NewObject(featureExpression.Type.GetConstructors().[0],allParams)

    ProvidedMethod("CreateFeature",[],featureExpression.Type,isStatic=true,invokeCode=fun _ -> feature)
    |> root.AddMember