namespace GherkinProvider.Builders

type ScenarioOutline<'S> (scenarioOutline:'S) as this =

    member private __.MakeName (examples:(string*string) list) =
        examples
        |> List.fold (fun a (header,value) -> sprintf "%s (%s is %s)" a header value ) "Example:"

    member private __.ReplaceExampleText (example:seq<obj>)  (txt:string) =
        example
        |> Seq.fold(fun (a:string) e ->
            let exampleType = e.GetType()
            let header = exampleType.GetProperty("Header").GetValue(e) |> string
            let value = exampleType.GetProperty("Value").GetValue(e) |> string
            exampleType.GetProperty("Visited").SetValue(e,true)
            let find =  sprintf "<%s>" header
            a.Replace(find,value))  txt 
        
    member private __.CopyStep (step:obj) (text:string) =
        let stepType = step.GetType()
        let keyword = stepType.GetProperty("Keyword").GetValue(step)
        let order = stepType.GetProperty("Order").GetValue(step)
        let argumentBase = 
            match stepType.GetProperty("Argument") with
            | null -> stepType.GetProperty("DocString").GetValue(step) 
            | arg -> arg.GetValue(step)

        let constructor = stepType.GetConstructors().[0]

        (constructor.Invoke([|text;keyword;order;argumentBase|])) 

    member private __.CopyScenario (scenario:obj) (steps:obj []) (name:string)=
        let scenarioType = scenario.GetType()
        let description = scenarioType.GetProperty("Description").GetValue(scenario)

        let tags =
            match scenarioType.GetProperty("Tags") with
            | null -> None
            | pi -> Some (pi.GetValue(scenario))

        let examples = scenarioType.GetProperty("Examples").GetValue(scenario)
        let constructor = scenarioType.GetConstructors().[0]

        let parameters = 
            match tags with
            | None -> [|(scenarioOutline :> obj);(name :> obj);description;examples |] |> Seq.toList
            | Some t -> [|(scenarioOutline :> obj);(name :> obj);description;t;examples |] |> Seq.toList
        let allParameters = parameters @ (steps |> Seq.toList) |> Seq.toArray

        (constructor.Invoke(allParameters)) :?> 'S

    member private __.ToScenarios (scenarioOutline:obj) =
        let scenarioType = scenarioOutline.GetType()
        let examples = scenarioType.GetProperty("Examples").GetValue(scenarioOutline) :?> System.Collections.Generic.IEnumerable<_>
        let steps = scenarioType.GetProperty("Steps").GetValue(scenarioOutline) :?> System.Collections.Generic.IEnumerable<_> |> Seq.toArray
        
        examples
        |> Seq.map(fun e->

            let exampleType = e.GetType()
            let cells = exampleType.GetProperty("Cells").GetValue(e) :?> System.Collections.Generic.IEnumerable<_>
            let replaceText = this.ReplaceExampleText cells

            let copiedSteps = 
                steps
                |> Array.map(fun step->
                    let txt = step.GetType().GetProperty("Text").GetValue(step) |> string
                    this.CopyStep step (txt |> replaceText))

            let name = 
                cells 
                |> Seq.map(fun c -> 
                     c.GetType().GetProperty("Header").GetValue(c) |> string,
                     c.GetType().GetProperty("Value").GetValue(c) |> string)  
                |> Seq.toList
                |>  this.MakeName
                         
            this.CopyScenario scenarioOutline copiedSteps name)
        |> Seq.cast<'S>
        |> Seq.toList
    
    member __.GetScenarioName(scenario:obj) = 
        scenario.GetType().GetProperty("Name").GetValue(scenario) |> string

    member this.ReturnFrom (x:'S->'T) =
        (this.GetScenarioName scenarioOutline),( this.ToScenarios(scenarioOutline) |> List.map (x))


