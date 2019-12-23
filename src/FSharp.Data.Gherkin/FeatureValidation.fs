namespace GherkinProvider.Validation

open System.Collections.Generic

type VisitedCell =
    {
        Header:string
        Value:string
        Summary:string
    }

type VisitedRow =
    {
        Index:int
        Cells:VisitedCell list
        Summary:string
    }

type VisitedStep =
    {
        Text:string
        Summary:string
        DocString:string
        DataTable:VisitedRow list
    }

type VisitedScenario =
    {
        Tags:string list
        Name:string
        Summary:string
        Steps:VisitedStep list
        Examples:VisitedRow list
    }

type VisitedRule = 
    {
        Name:string
        Summary:string
        Examples:VisitedScenario list
    }   

type VisitedFeature =
    {
        Background:VisitedScenario option
        Tags:string list
        Scenarios:VisitedScenario list
        Rules:VisitedRule list
        Summary:string
    }

type FeatureValidator() as this = 

    member private __.Exclude (ignoreTags:string list) (tagList:obj) = 
            let tagListType = tagList.GetType()
            let tags = tagListType.GetProperty("AllTags").GetValue(tagList) :?> IEnumerable<_>

            tags
            |> Seq.exists(fun tag ->
                 let tagType = tag.GetType()
                 let tagName = tagType.GetProperty("Name").GetValue(tag) :?> string

                 ignoreTags |> Seq.exists(fun i -> i = tagName))

    member private __.NonVisitedDataTable (dataTable:obj) =
            if isNull dataTable then []
            else
                dataTable :?> IEnumerable<_>
                |> Seq.mapi(fun i row ->
                    let dataRowType = row.GetType()
                    let cells = dataRowType.GetProperty("Cells").GetValue(row) :?> IEnumerable<_> |> Seq.rev
                    
                    let nonVisitedCells = 
                        cells 
                        |> Seq.choose(fun cell ->
                            let cellType = cell.GetType()
                            let visited = cellType.GetProperty("Visited").GetValue(cell) :?> bool
                            let header = cellType.GetProperty("Header").GetValue(cell) :?> string
                            let value = cellType.GetProperty("Value").GetValue(cell) :?> string

                            if visited then None
                            else 
                             {
                                Header = header
                                Value = value
                                Summary = sprintf "         DataCell: %s:%s" header value
                             } |> Some)
                        |> Seq.toList

                    match nonVisitedCells with
                    | [] -> None
                    | _ ->
                        {
                           Cells = nonVisitedCells
                           Index = i
                           Summary = nonVisitedCells |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary ) (sprintf "     DataRow %i:" i)
                        } |> Some)
                |> Seq.choose(id)
                |> Seq.toList

    member private __.DocStringVisited (step:obj) =
            let stepType = step.GetType()
            let text = stepType.GetProperty("Text").GetValue(step) :?> string
            let docString= stepType.GetProperty("DocString").GetValue(step)

            if isNull docString then ""
            else
                let docStringType = docString.GetType()
                if not (docStringType.GetProperty("Visited").GetValue(docString) :?> bool) then  sprintf "  %s\r\n  DocString" text
                else ""

    member private __.NonVisitedSteps (scenario:obj) =
            let scenarioType = scenario.GetType()
            let steps = scenarioType.GetProperty("Steps").GetValue(scenario) :?> IEnumerable<_>

            steps
            |> Seq.choose(fun step ->
                let stepType = step.GetType()
                let visited = stepType.GetProperty("Visited").GetValue(step) :?> bool
                let text = stepType.GetProperty("Text").GetValue(step) :?> string
                let docStringVisited = this.DocStringVisited step
                let dataTableVisited = this.NonVisitedDataTable (stepType.GetProperty("DataTable").GetValue(step))

                match visited,docStringVisited.Length = 0,dataTableVisited.Length = 0 with
                | true,true,true -> None
                | _ ->
                       Some
                        {
                            DocString = docStringVisited
                            Text = text
                            Summary = sprintf " Step:%s%s%s" text docStringVisited (dataTableVisited |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary) "")
                            DataTable= dataTableVisited
                        })
            |> Seq.toList

    member __.NonVisitedTags ignoreTags tagsList =
                tagsList 
                |> Seq.filter(fun tag ->
                     let tagType = tag.GetType()
                     let tagName = tagType.GetProperty("Name").GetValue(tag) :?> string

                     not (ignoreTags |> Seq.exists(fun i -> i = tagName)) &&
                     not (tagType.GetProperty("Visited").GetValue(tag) :?> bool))
                |> Seq.map(fun tag ->
                    let tagType = tag.GetType()
                    tagType.GetProperty("Name").GetValue(tag) :?> string)
                |> Seq.toList

    member __.GetNonVisitedScenarios ignoreTags scenarios isScenario= 
            let typeName = if isScenario then "Scenario" else "Background"
            scenarios 
            |> Seq.choose(fun scenario ->
                let scenarioType = scenario.GetType()
                let scenarioVisited = scenarioType.GetProperty("Visited").GetValue(scenario) :?> bool
                let name = scenarioType.GetProperty("Name").GetValue(scenario) :?> string
                let examples = scenarioType.GetProperty("ExampleTable").GetValue(scenario)
                let steps = this.NonVisitedSteps scenario
                
                let tagContainer = scenarioType.GetProperty("TagList").GetValue(scenario) 
                
                if this.Exclude ignoreTags tagContainer then None
                else
                    let tagContainerType = tagContainer.GetType()
                    let tags = tagContainerType.GetProperty("AllTags").GetValue(tagContainer) :?> IEnumerable<_>
                    let nonVisitedTags = this.NonVisitedTags ignoreTags tags
                    let nonVisitedExamples = this.NonVisitedDataTable examples

                    let visited =
                        if not isScenario then true
                        else isScenario && scenarioVisited

                    match visited,steps,nonVisitedExamples.Length = 0,nonVisitedTags.Length =0 with
                    | true,[],true,true -> None
                    | _ ->
                        let summary = 
                            let scenarioSummary = steps |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary ) (sprintf "\r\n%s:%s" typeName name)

                            let examplesSummary = 
                                if nonVisitedExamples.Length > 0 then
                                        nonVisitedExamples 
                                        |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary) " Examples:"
                                else ""

                            let tagsSummary = 
                                    if nonVisitedTags.Length > 0 then
                                            nonVisitedTags 
                                            |> Seq.fold(fun a c -> sprintf "%s %s" a c) " Tags:"
                                    else ""

                            sprintf "%s\r\n%s\r\n%s" scenarioSummary tagsSummary examplesSummary

                        Some
                            {
                                Tags = nonVisitedTags
                                Name =  name
                                Summary = summary
                                Steps = steps
                                Examples = this.NonVisitedDataTable examples
                            })
            |> Seq.toList

    member __.Validate (feature:obj,?ignore:string list) = 
        let ignoreTags = if ignore.IsSome then ignore.Value else []
        
        let featureType=feature.GetType()
        let scenarioContainer = featureType.GetProperty("Scenarios").GetValue(feature)
        let scenarioContainerType = scenarioContainer.GetType()
        let scenarios = scenarioContainerType.GetProperty("All").GetValue(scenarioContainer) :?> IEnumerable<_>
        
        
        let featureTags = 
            match featureType.GetProperty("Tags") with
            | null -> Seq.empty
            | tags ->
                let allTags = tags.GetValue(feature)
                let allTagsType = allTags.GetType()

                allTagsType.GetProperty("AllTags").GetValue(allTags) :?> IEnumerable<_>


        if not (isNull (featureType.GetProperty("Tags"))) && this.Exclude ignoreTags (featureType.GetProperty("Tags").GetValue(feature)) then None
        else
            let nonVisitedFeatureTags = this.NonVisitedTags ignoreTags featureTags
            let nonVisitedScenarios= this.GetNonVisitedScenarios ignoreTags scenarios true
            let nonVisitedBackground= 
                match featureType.GetProperty("Background")  with
                | null -> None
                | bgProperty -> 
                    let bg = bgProperty.GetValue(feature)
                    match this.GetNonVisitedScenarios ignoreTags [bg] false with
                    |[] -> None
                    |x :: _ -> Some x


            let backgroundSummary = if nonVisitedBackground.IsSome then nonVisitedBackground.Value.Summary else ""
            let tagsSummary = nonVisitedFeatureTags |> Seq.fold(fun a c -> sprintf "    Tag:%s\r\n%s" c a) ""
            let scenariosSummary = nonVisitedScenarios |> Seq.fold(fun a c -> sprintf "%s\r\n%s" c.Summary a) ""

            match nonVisitedFeatureTags,nonVisitedScenarios,nonVisitedBackground with
            | [],[],None -> None
            | _ ->
                {
                    Background = nonVisitedBackground
                    Tags = nonVisitedFeatureTags
                    Scenarios = nonVisitedScenarios
                    Rules = []
                    Summary = sprintf "\r\nFeature:\r\n%s%s%s" tagsSummary backgroundSummary scenariosSummary
                } |> Some
