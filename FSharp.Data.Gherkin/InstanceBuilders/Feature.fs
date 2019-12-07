module InstanceBuilders.Feature

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast
open ExpressionBuilders

let buildRows  (rowType:ProvidedTypeDefinition) (dataRow:TableRow list) =
        dataRow 
        |> List.map(
            fun dr -> 
                let parameters =
                    dr.Cells |> Seq.toList 
                    |> List.map (fun c -> Expr.Value(c.Value))
                Expr.NewObject(rowType.GetConstructors().[0],parameters))

let buildExamples (examples:Examples list) (exampleType:ProvidedTypeDefinition) =
    let rows=
        examples 
        |> List.collect(fun e -> e.TableBody |> Seq.toList)
        |> buildRows exampleType
    Expr.NewArray(exampleType,rows)

let buildArgument (argument:Gherkin.Ast.StepArgument) (stepType:StepExpression) =
    match argument,stepType.Argument with
    | :? DocString,Some argType -> 
        let docString = argument :?> DocString
        let content = Expr.Value(docString.Content)
        let contentType = Expr.Value(docString.ContentType)

        Some (Expr.NewObject(argType.GetConstructors().[0],[content;contentType]))
    | :? DataTable,Some argType -> 
        let dataTable = argument :?> DataTable
        let dataTableRows = buildRows argType (dataTable.Rows |> Seq.toList) 
        Some (Expr.NewArray(argType,dataTableRows))
    | _ -> None

let buildSteps (steps:Step list) (stepsType:StepExpression list) =
    List.mapi2(
        fun i (step:Step) (stepType:StepExpression) ->
            let parameters = 
                let text = step.Text.Trim()
                let keyword = step.Keyword.Trim()
                match buildArgument step.Argument stepType with
                | None -> 
                    [
                        Expr.Value(text)
                        Expr.Value(keyword)
                        Expr.Value(i)
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

let buildScenarios (scenarios:Scenario list) (scenarioTypes:ScenarioExpression list) =

    List.map2(
        fun (scenario:Scenario) (scenarioType:ScenarioExpression) ->
            let name = Expr.Value(scenario.Name)
            let description = Expr.Value(scenario.Description)
            let steps = buildSteps (scenario.Steps |> Seq.toList) scenarioType.Steps

            let parameters = 
                match scenarioType.Examples,(scenario.Tags |> Seq.toList)  with
                | None,[] -> 
                    name :: description :: steps

                | Some exampleType,[] ->
                    let examples = buildExamples (scenario.Examples |> Seq.toList) exampleType
                    name :: description :: examples :: steps

                | None,tags ->
                    match scenarioType.Tags with
                    | None -> name :: description :: steps
                    | Some tagType ->
                        let tagsInstance = createTagInstance tagType tags
                        name :: description :: tagsInstance :: steps

                | Some exampleType,tags ->

                    let examples = buildExamples (scenario.Examples |> Seq.toList) exampleType

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

let buildFeatureInstance (root:ProvidedTypeDefinition) (gherkinDocument:GherkinDocument) (featureExpression:FeatureExpression) =

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

            let steps =  buildSteps (gherkinBackground.Steps |> Seq.toList) bgType.Steps
            let background = buildBackground bgType.Type gherkinBackground steps
            
            [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background]

         | Some bgType,Some gherkinBackground,tags ->
            
            let steps =  buildSteps (gherkinBackground.Steps |> Seq.toList) bgType.Steps
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

    let scenarios = buildScenarios gherkinScenarios featureExpression.Scenarios

    let allParams = parameters @ scenarios

    let feature = Expr.NewObject(featureExpression.Type.GetConstructors().[0],allParams)

    ProvidedMethod("CreateFeature",[],featureExpression.Type,isStatic=true,invokeCode=fun _ -> feature)
    |> root.AddMember