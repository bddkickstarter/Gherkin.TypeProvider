namespace GherkinProvider.Validation

open System.Collections.Generic

type UnVisitedCell =
    {
        Header:string
        Value:string
        Summary:string
    }

type UnVisitedRow =
    {
        Index:int
        Cells:UnVisitedCell list
        Summary:string
    }

type UnVisitedStep =
    {
        Text:string
        Summary:string
        DocString:string
        DataTable:UnVisitedRow list
    }

type UnVisitedScenario =
    {
        Tags:string list
        Name:string
        Summary:string
        Steps:UnVisitedStep list
        Examples:UnVisitedRow list
    }

type UnVisitedRule = 
    {
        Name:string
        Summary:string
        Examples:UnVisitedScenario list
    }   

type UnVisitedFeature =
    {
        Background:UnVisitedScenario option
        Tags:string list
        Scenarios:UnVisitedScenario list
        Rules:UnVisitedRule list
        Summary:string
    }
    
type ChildType =
    |Background
    |Scenario

type FeatureValidator() as this = 

    member private __.Exclude (ignoreTags:string list) (tagList:obj) = 
            let tagListType = tagList.GetType()
            let tags = tagListType.GetProperty("AllTags").GetValue(tagList) :?> IEnumerable<_>

            tags
            |> Seq.exists(fun tag ->
                 let tagType = tag.GetType()
                 let tagName = tagType.GetProperty("Name").GetValue(tag) :?> string

                 ignoreTags |> Seq.exists(fun i -> i = tagName))

    member private __.GetUnVisitedDataTable (dataTable:obj) =
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

    member private __.GetUnVisitedDocString (step:obj) =
            let stepType = step.GetType()
            let text = stepType.GetProperty("Text").GetValue(step) :?> string
            let docString= stepType.GetProperty("DocString").GetValue(step)

            if isNull docString then ""
            else
                let docStringType = docString.GetType()
                if not (docStringType.GetProperty("Visited").GetValue(docString) :?> bool) then "DocString"
                else ""

    member private __.GetUnVisitedSteps (scenario:obj) =
            let scenarioType = scenario.GetType()
            let steps = scenarioType.GetProperty("Steps").GetValue(scenario) :?> IEnumerable<_>

            steps
            |> Seq.choose(fun step ->
                let stepType = step.GetType()
                let visited = stepType.GetProperty("Visited").GetValue(step) :?> bool
                let text = stepType.GetProperty("Text").GetValue(step) :?> string
                let docStringVisited = this.GetUnVisitedDocString step
                let dataTableVisited = this.GetUnVisitedDataTable (stepType.GetProperty("DataTable").GetValue(step))

                match visited,docStringVisited.Length = 0,dataTableVisited.Length = 0 with
                | true,true,true -> None
                | _ ->
                       let summary = sprintf " Step:%s\r\n%s%s" text docStringVisited (dataTableVisited |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary) "")
                       Some
                        {
                            DocString = docStringVisited
                            Text = text
                            Summary = summary
                            DataTable= dataTableVisited
                        })
            |> Seq.toList

    member __.GetUnVisitedTags ignoreTags tagsList =
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

    member __.GetUnVisitedScenarios ignoreTags  (childType:ChildType) scenarios = 

            scenarios 
            |> Seq.choose(fun scenario ->
                let scenarioType = scenario.GetType()
                let scenarioVisited = scenarioType.GetProperty("Visited").GetValue(scenario) :?> bool
                let name = scenarioType.GetProperty("Name").GetValue(scenario) :?> string
                let examples = scenarioType.GetProperty("ExampleTable").GetValue(scenario)
                let steps = this.GetUnVisitedSteps scenario
                
                let tagContainer = scenarioType.GetProperty("TagList").GetValue(scenario) 
                
                if this.Exclude ignoreTags tagContainer then None
                else
                    let tagContainerType = tagContainer.GetType()
                    let tags = tagContainerType.GetProperty("AllTags").GetValue(tagContainer) :?> IEnumerable<_>
                    let nonVisitedTags = this.GetUnVisitedTags ignoreTags tags
                    let nonVisitedExamples = this.GetUnVisitedDataTable examples

                    let visited =
                        match childType with
                        | Background -> true
                        | Scenario -> scenarioVisited

                    match visited,steps,nonVisitedExamples.Length = 0,nonVisitedTags.Length =0 with
                    | true,[],true,true -> None
                    | _ ->
                        let summary = 
                            let scenarioSummary = 
                                steps 
                                |> List.fold(fun a c -> 
                                    sprintf "%s\r\n%s" a c.Summary) 
                                    (sprintf "\r\n%A:%s" childType name)

                            let examplesSummary = 
                                if nonVisitedExamples.Length > 0 then
                                        nonVisitedExamples 
                                        |> List.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary) " Examples:"
                                else ""

                            let tagsSummary = 
                                    if nonVisitedTags.Length > 0 then
                                            nonVisitedTags 
                                            |> List.fold(fun a c -> sprintf "%s %s" a c) " Tags:"
                                    else ""

                            sprintf "%s\r\n%s\r\n%s" scenarioSummary tagsSummary examplesSummary

                        Some
                            {
                                Tags = nonVisitedTags
                                Name =  name
                                Summary = summary
                                Steps = steps
                                Examples = this.GetUnVisitedDataTable examples
                            })
            |> Seq.toList
            
    member private __.GetUnVisitedRules ignoreTags rules =
         rules 
            |> Seq.rev
            |> Seq.filter(fun rule -> not (rule.GetType().GetProperty("Visited").GetValue(rule) :?> bool))
            |> Seq.map(
                fun unvisitedRule ->
                    let unvisitedRuleType = unvisitedRule.GetType()
                    let name =  unvisitedRuleType.GetProperty("Name").GetValue(unvisitedRule) :?> string
                    let examples =
                            unvisitedRuleType.GetProperty("All").GetValue(unvisitedRule)  :?> IEnumerable<_>
                            |> Seq.toList
                            |> this.GetUnVisitedScenarios ignoreTags Scenario
                    
                    let summary =
                        sprintf
                            " Rules:%s\r\n\r\n%s"
                            name
                            (examples |> Seq.fold(
                                fun a c -> 
                                    sprintf "%s\r\n %s" a c.Summary ) "  Examples:")
                    
                    {
                        Name =name
                        Summary = summary
                        Examples =examples
                    })
            |> Seq.toList

    member __.Validate (feature:obj,?ignore:string list) = 
        let ignoreTags = if ignore.IsSome then ignore.Value else []
        
        let featureType=feature.GetType()
        
        //rules
        let ruleContainer = featureType.GetProperty("Rules").GetValue(feature)
        let ruleContainerType = ruleContainer.GetType()
        let rules = ruleContainerType.GetProperty("All").GetValue(ruleContainer) :?> IEnumerable<_>
        
        //scenarios
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
            let unVisitedFeatureTags = this.GetUnVisitedTags ignoreTags featureTags
            let unVisitedRules= this.GetUnVisitedRules ignoreTags rules
            let unVisitedScenarios= this.GetUnVisitedScenarios ignoreTags ChildType.Scenario scenarios
            
            let unVisitedBackground= 
                match featureType.GetProperty("Background")  with
                | null -> None
                | bgProperty -> 
                    let bg = bgProperty.GetValue(feature)
                    match this.GetUnVisitedScenarios ignoreTags ChildType.Background [bg] with
                    |[] -> None
                    |x :: _ -> Some x


            let backgroundSummary = if unVisitedBackground.IsSome then unVisitedBackground.Value.Summary else ""
            let tagsSummary = unVisitedFeatureTags |> Seq.fold(fun a c -> sprintf "    Tag:%s\r\n%s" c a) ""
            let scenariosSummary = unVisitedScenarios |> Seq.fold(fun a c -> sprintf "%s\r\n%s" c.Summary a) ""
            let rulesSummary = unVisitedRules |> Seq.fold(fun a c -> sprintf "%s\r\n%s" c.Summary a) ""

            match unVisitedFeatureTags,unVisitedScenarios,unVisitedRules, unVisitedBackground with
            | [],[],[],None -> None
            | _ ->
                {
                    Background = unVisitedBackground
                    Tags = unVisitedFeatureTags
                    Scenarios = unVisitedScenarios
                    Rules = unVisitedRules
                    Summary = sprintf "\r\nFeature:\r\n%s%s%s%s" tagsSummary backgroundSummary rulesSummary scenariosSummary
                } |> Some
