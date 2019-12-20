module InstanceBuilders.Feature

open BaseTypes.Scenario
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast
open ObjectModel

type RowBuilder (dataCellType:System.Type) =

    member __.BuildRows (columns:string list) (rowType:ProvidedTypeDefinition) (dataRow:TableRow list) = 
        dataRow 
        |> List.map(
            fun dr -> 
                let parameters =
                    List.map2 (
                        fun (column:string) (cell:TableCell) -> 
                            Expr.NewObject(
                                dataCellType.GetConstructors().[0],
                                [Expr.Value(column);Expr.Value(cell.Value)])) columns (dr.Cells |> Seq.toList) 

                Expr.NewObject(rowType.GetConstructors().[0],parameters))

type ExampleBuilder (rowBuilder:RowBuilder) =
    
    member __.BuildExamples (examples:Examples list) (exampleType:ProvidedTypeDefinition) = 
        match examples with
        | [] -> Expr.NewArray(exampleType,[])
        | examplesList ->
            let headers = examplesList.[0].TableHeader.Cells |> Seq.toList |> List.map(fun th -> th.Value)
            let rows=
                examplesList 
                |> List.collect(fun e -> e.TableBody |> Seq.toList)
                |> rowBuilder.BuildRows headers exampleType

            Expr.NewArray(exampleType,rows)

type ArgumentBuilder (rowBuilder:RowBuilder) =

    member __.BuildArgument (argument:Gherkin.Ast.StepArgument) (stepType:StepExpression) =
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
                let dataTableRows = rowBuilder.BuildRows headers dataTableType rows
                
                Some (Expr.NewArray(dataTableType,dataTableRows))
            | _ -> None
        | _ -> None

type StepsBuilder (argumentBuilder:ArgumentBuilder,argumentBaseType:System.Type) =

    member __.BuildSteps (steps:Step list) (stepsType:StepExpression list) =
        List.mapi2(
            fun i (step:Step) (stepType:StepExpression) ->
                let parameters = 
                    let text = step.Text.Trim()
                    let keyword = step.Keyword.Trim()
                    match argumentBuilder.BuildArgument step.Argument stepType with
                    | None -> 
                        [
                            Expr.Value(text)
                            Expr.Value(keyword)
                            Expr.Value(i)
                            Expr.Coerce(Expr.Value(null),argumentBaseType)
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

type TagBuilder (tagType:ProvidedTypeDefinition) =

    member __.BuildTag(tagContainerType:ProvidedTypeDefinition) (tags:Tag list) =
        let tagInstances = tags |> List.map(fun t -> Expr.NewObject(tagType.GetConstructors().[0],[Expr.Value(t.Name)]))
        Expr.NewObject(tagContainerType.GetConstructors().[0],tagInstances)

type ScenarioBuilder (parent:Expr, exampleBuilder:ExampleBuilder,tagBuilder:TagBuilder,stepsBuilder:StepsBuilder)  =

    member __.BuildScenario (scenarios:Scenario list) (scenarioTypes:ScenarioExpression list) =
        List.map2(
            fun (scenario:Scenario) (scenarioType:ScenarioExpression) ->
                let name = Expr.Value(scenario.Name)
                let description = Expr.Value(scenario.Description)
                let steps = stepsBuilder.BuildSteps (scenario.Steps |> Seq.toList) scenarioType.Steps
                let parameters = 
                    match scenarioType.Examples,(scenario.Tags |> Seq.toList)  with
                    | None,[] -> 
                        parent :: name :: description :: steps

                    | Some exampleType,[] ->
                        let examples = exampleBuilder.BuildExamples (scenario.Examples |> Seq.toList) exampleType
                        parent :: name :: description :: examples :: steps

                    | None,tags ->
                        match scenarioType.Tags with
                        | None -> parent :: name :: description :: steps
                        | Some tagType ->
                            let tagsInstance = tagBuilder.BuildTag tagType tags
                            parent :: name :: description :: tagsInstance :: steps

                    | Some exampleType,tags ->

                        let examples = exampleBuilder.BuildExamples (scenario.Examples |> Seq.toList) exampleType

                        match scenarioType.Tags with
                        | None -> parent :: name :: description :: examples :: steps
                        | Some tagType ->
                            let tagsInstance = tagBuilder.BuildTag tagType tags
                            parent :: name :: description :: tagsInstance :: examples ::  steps

                Expr.NewObject(scenarioType.Type.GetConstructors().[0],parameters)
        ) scenarios scenarioTypes

type BackgroundBuilder (parent:Expr) =

    member __.BuildBackground (backgroundType:ProvidedTypeDefinition) (gherkinBackground:Background) (steps:Expr list) =
        let nameParam = Expr.Value(gherkinBackground.Name)
        let descriptionParam = Expr.Value(gherkinBackground.Description)
        let parameters = parent :: nameParam :: descriptionParam :: steps

        Expr.NewObject(backgroundType.GetConstructors().[0],parameters)

type FeatureBuilder (stepsBuilder:StepsBuilder,backgroundBuilder:BackgroundBuilder,tagBuilder:TagBuilder,scenarioBuilder:ScenarioBuilder)  =

    member __.BuildFeature (root:ProvidedTypeDefinition) (gherkinDocument:GherkinDocument) (featureExpression:FeatureExpression) =
        let gherkinScenarios =
            gherkinDocument.Feature.Children |> Seq.toList
            |> List.choose(
                fun c ->
                    match c with
                    | :? Scenario -> Some  (c :?> Scenario)
                    | _ -> None)

        let gherkinBackground = 
            gherkinDocument.Feature.Children 
                |> Seq.tryPick(
                    fun c ->
                        match c with
                        | :? Background -> Some  (c :?> Background)
                        | _ -> None)

        let parameters = 
             match featureExpression.Background,gherkinBackground,(gherkinDocument.Feature.Tags |> Seq.toList) with

             | Some bgType,Some gherkinBackground,[] ->

                let steps =  stepsBuilder.BuildSteps (gherkinBackground.Steps |> Seq.toList) bgType.Steps
                let background = backgroundBuilder.BuildBackground bgType.Type gherkinBackground steps
                
                [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background]

             | Some bgType,Some gherkinBackground,tags ->
                
                let steps =  stepsBuilder.BuildSteps (gherkinBackground.Steps |> Seq.toList) bgType.Steps
                let background = backgroundBuilder.BuildBackground bgType.Type gherkinBackground steps

                match featureExpression.Tags with
                | None ->  
                    [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background]
                | Some featureTagType -> 
                    let tagsInstance = tagBuilder.BuildTag featureTagType tags
                    [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);background;tagsInstance]

             | None, _,tags ->
                
                match featureExpression.Tags with
                | Some featureTagType -> 
                    let tagsinstance = tagBuilder.BuildTag featureTagType tags
                    [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description);tagsinstance]
                | None -> [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description)]

             | _ -> [Expr.Value(gherkinDocument.Feature.Name);Expr.Value(gherkinDocument.Feature.Description)]

        let scenarios = scenarioBuilder.BuildScenario gherkinScenarios featureExpression.Scenarios
        let allParams = parameters @ scenarios
        let feature = Expr.NewObject(featureExpression.Type.GetConstructors().[0],allParams)

        ProvidedMethod("CreateFeature",[],featureExpression.Type,isStatic=true,invokeCode=fun _ -> feature)
        |> root.AddMember

    static member CreateNew (providerModel:GherkinProviderModel) =
            let parent = Expr.Coerce(Expr.Value(null),providerModel.ScenarioBaseType)

            let rowBuilder = RowBuilder(providerModel.DataCellBaseType)
            let exampleBuilder = ExampleBuilder(rowBuilder)
            let argumentBuilder = ArgumentBuilder(rowBuilder)
            let stepsBuilder = StepsBuilder(argumentBuilder,providerModel.ArgumentBaseType)
            let tagBuilder = TagBuilder(providerModel.TagBaseType)
            let scenarioBuilder = ScenarioBuilder(parent,exampleBuilder,tagBuilder,stepsBuilder)
            let backgroundBuilder = BackgroundBuilder(parent)
            FeatureBuilder(stepsBuilder,backgroundBuilder,tagBuilder,scenarioBuilder)
