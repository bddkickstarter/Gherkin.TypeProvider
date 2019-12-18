namespace FSharp.Data.Gherkin.Validation

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
        //Tags:string list
        Name:string
        Summary:string
        Steps:VisitedStep list
        Examples:VisitedRow list
    }

type VisitedFeature =
    {
        Tags:string list
        Scenarios:VisitedScenario list
        Summary:string
    }

type FeatureValidator() = 

    static member Validate (feature:obj) = 
        
        let featureType=feature.GetType()
        let scenarios = featureType.GetProperty("Scenarios").GetValue(feature) :?> IEnumerable<_>
        
        let featureTags = 
            match featureType.GetProperty("Tags") with
            | null -> Seq.empty
            | tags ->
                let allTags = tags.GetValue(feature)
                let allTagsType = allTags.GetType()

                allTagsType.GetProperty("AllTags").GetValue(allTags) :?> IEnumerable<_>


        let docStringVisited (step:obj) =
            let stepType = step.GetType()
            let text = stepType.GetProperty("Text").GetValue(step) :?> string
            let docString= stepType.GetProperty("DocString").GetValue(step)

            if isNull docString then ""
            else
                let docStringType = docString.GetType()
                if not (docStringType.GetProperty("Visited").GetValue(docString) :?> bool) then  sprintf "  %s\r\n  DocString" text
                else ""

        let nonVisitedDataTable (dataTable:obj) =
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


        let nonVisitedSteps (scenario:obj) =
            let scenarioType = scenario.GetType()
            let steps = scenarioType.GetProperty("Steps").GetValue(scenario) :?> IEnumerable<_>

            steps
            |> Seq.choose(fun step ->
                let stepType = step.GetType()
                let visited = stepType.GetProperty("Visited").GetValue(step) :?> bool
                let text = stepType.GetProperty("Text").GetValue(step) :?> string
                let docStringVisited = docStringVisited step
                let dataTableVisited = nonVisitedDataTable (stepType.GetProperty("DataTable").GetValue(step))

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

        let nonVisitedTags tagsList=
            tagsList 
            |> Seq.filter(fun tag ->
                 let tagType = tag.GetType()
                 not (tagType.GetProperty("Visited").GetValue(tag) :?> bool))
            |> Seq.map(fun tag ->
                let tagType = tag.GetType()
                tagType.GetProperty("Name").GetValue(tag) :?> string)
            |> Seq.toList
        
        let nonVisitedScenarios = 
            scenarios 
            |> Seq.choose(fun scenario ->
                let scenarioType = scenario.GetType()
                let visited = scenarioType.GetProperty("Visited").GetValue(scenario) :?> bool
                let name = scenarioType.GetProperty("Name").GetValue(scenario) :?> string
                let examples = scenarioType.GetProperty("ExampleTable").GetValue(scenario)
                let steps = nonVisitedSteps scenario
                //TODO!!
                //let tags = scenarioType.GetProperty("AllTags").GetValue(scenario) :?> IEnumerable<_>
                //let nonVisitedTags = nonVisitedTags tags

                let nonVisitedExamples = nonVisitedDataTable examples

                match visited,steps,nonVisitedExamples.Length = 0 with//,nonVisitedTags with
                | true,[],true -> None
                | _ ->
                    let summary = 
                        let scenarioSummary = steps |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary ) (sprintf "\r\nScenario:%s" name)
                        if nonVisitedExamples.Length > 0 then
                                let examplesSummary =
                                    nonVisitedExamples 
                                    |> Seq.fold(fun a c -> sprintf "%s\r\n%s" a c.Summary) " Examples:"
                                sprintf "%s\r\n%s" scenarioSummary examplesSummary
                        else scenarioSummary

                    Some
                        {
                            Name =  name
                            Summary = summary
                            Steps = steps
                            Examples = nonVisitedDataTable examples
                        })
            |> Seq.toList

        let nonVisitedFeatureTags = nonVisitedTags featureTags
        let tagsSummary = nonVisitedFeatureTags |> Seq.fold(fun a c -> sprintf "    Tag:%s\r\n%s" c a) ""
        let scenariosSummary = nonVisitedScenarios |> Seq.fold(fun a c -> sprintf "%s\r\n%s" c.Summary a) ""

        match nonVisitedFeatureTags,nonVisitedScenarios with
        | [],[] -> None
        | _ ->
            {
                Tags = nonVisitedFeatureTags
                Scenarios = nonVisitedScenarios
                Summary = sprintf "%s\r\n%s" tagsSummary scenariosSummary
            } |> Some
