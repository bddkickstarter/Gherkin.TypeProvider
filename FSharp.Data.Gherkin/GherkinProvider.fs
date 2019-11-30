namespace FSharp.Data.Gherkin.ProviderImplementation

open FSharp.Data.Gherkin
open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open ProviderImplementation.ProvidedTypes

open Gherkin

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "FSharp.Data.Gherkin"
    let parser = Parser()
    let asm = Assembly.GetExecutingAssembly()

    let createDataInstance (column:string) (value:Expr) = Expr.NewObject(Constructors.DataCell,[Expr.Value(column);value])
    let createDefaultInstance _ (value:Expr) = value
    
    let createDynamicObject name propertyNames propertyType createInstance =
        let dynamicObjectType  = ProvidedTypeDefinition(name,Some typeof<obj>, isErased=false, hideObjectMethods=true, nonNullable=true)

        let parameters = 
            propertyNames 
            |> List.map(fun h -> ProvidedParameter(h,typeof<string>))
        
        let fields = 
            propertyNames 
            |> List.map(fun h -> ProvidedField( sprintf "_%s" h,propertyType))

        fields |> Seq.iter (dynamicObjectType.AddMember)

        let properties =
            propertyNames
            |> Seq.mapi(fun i h -> ProvidedProperty(h,propertyType,getterCode=fun args -> Expr.FieldGet(args.[0],fields.[i])))

        properties |> Seq.iter (dynamicObjectType.AddMember)
        
        let ctr =
            ProvidedConstructor(parameters,
                fun args -> 
                    match args with
                    | this :: xs -> 
                        match xs with
                        | e :: ex when ex.Length = 0 -> Expr.FieldSet(this,fields.[0],createInstance propertyNames.[0] e)
                        | e :: ex when ex.Length <> 0 -> 
                            let first =  Expr.FieldSet(this,fields.[0],(createInstance propertyNames.[0] e))
                            let rest = ex |> List.mapi(fun i expr -> Expr.FieldSet(this,fields.[i+1],(createInstance propertyNames.[i+1] expr)))
                            rest |> List.fold (fun a c -> Expr.Sequential(a,c)) first
                            
                        | _ -> failwith ("incorrect constructor arguments")
                    | _ -> failwith ("incorrect constructor arguments"))
        
        ctr |> dynamicObjectType.AddMember
        dynamicObjectType

    let createExamplesFromRows (ctr:ConstructorInfo) (rows:seq<Ast.TableRow>) =
        rows
        |> Seq.map (fun r -> r.Cells |> Seq.map (fun c -> Expr.Value(c.Value,typeof<string>)) |> Seq.toList)
        |> Seq.map (fun args -> Expr.NewObject(ctr,args))
        |> Seq.toList

    let addArgument (arg:Ast.StepArgument) (step:ProvidedTypeDefinition) =

        let createDataInstances (rows:seq<Ast.TableRow>) (exampleType:ProvidedTypeDefinition) = 
            let ctr = exampleType.GetConstructors().[0]
            let examples = createExamplesFromRows ctr rows
            Expr.NewArray(exampleType,examples)
            
        if isNull arg then step
        else
            
            match arg with
            | :? Ast.DataTable -> 
                let dataTable = arg :?> Ast.DataTable
                let rows = dataTable.Rows |> Seq.toList
                let propertyNames = rows.Head.Cells |> Seq.map(fun c -> c.Value) |> Seq.toList

                let rowType  = createDynamicObject "Row" propertyNames typeof<DataCell> createDataInstance

                rowType |> step.AddMember

                let addRows _ = createDataInstances rows.Tail rowType
                let rowsType =  (typedefof<seq<_>>).MakeGenericType(rowType.AsType())
                let argument = ProvidedProperty("Data",rowsType,isStatic=false,getterCode=addRows)
                argument |> step.AddMember

                step
            | :? Ast.DocString ->
                let docString = arg :?> Ast.DocString
                let header = ["Content";"ContentType"]
                let docStringType = createDynamicObject "DocString" header typeof<string> createDefaultInstance
                docStringType |> step.AddMember
                
                let docStringObj = Expr.NewObject(docStringType.GetConstructors().[0],[Expr.Value(docString.Content);Expr.Value(docString.ContentType)])
                let argument = ProvidedProperty("Argument",docStringType,isStatic=false,getterCode=fun _ -> docStringObj)
                argument |> step.AddMember
                step

            | _ -> step

    let createTags (tags:seq<Ast.Tag>) = 

        match tags |> Seq.toList with
        | [] -> None
        | tagList ->
            
            let tagExprs = 
                tagList
                |> List.map(fun t -> Expr.Value(t.Name))
        
            let tagsInstance = Expr.NewArray(typeof<string>,tagExprs)

            Some (ProvidedProperty("Tags",typeof<seq<string>>,isStatic = false, getterCode = fun _ -> tagsInstance ))

    let createStep (order:int) (step:Ast.Step) (parent:ProvidedTypeDefinition) =
        let text = step.Text
        let keyword = step.Keyword.Trim()
        let name = sprintf "%i. %s %s" order keyword text
        let stepType = ProvidedTypeDefinition(name, Some typeof<Step>, isErased=false, hideObjectMethods=true, nonNullable=true) |> addArgument step.Argument
        let step = Expr.NewObject(Constructors.Step,[Expr.Value(order);Expr.Value(keyword);Expr.Value(text)])
        stepType |> parent.AddMember 
        ProvidedProperty(name,stepType.AsType(),isStatic = false, getterCode=fun _ -> step) |> parent.AddMember
        step
    
    let createScenarioType (gherkinScenario:Ast.Scenario) =
        let scenario = ProvidedTypeDefinition(gherkinScenario.Name, Some typeof<Scenario>, isErased=false, hideObjectMethods=true, nonNullable = true)

        let steps = 
            gherkinScenario.Steps
            |> Seq.mapi(fun i s -> createStep i s scenario)
            |> Seq.toList

        (scenario,Expr.NewArray(typeof<Step>,steps))

    let createBackgroundType (gherkinBackground:Ast.Background) =
        let background = ProvidedTypeDefinition(gherkinBackground.Name, Some typeof<Background>, isErased=false, hideObjectMethods=true, nonNullable=true)
        
        let steps =
            gherkinBackground.Steps
            |> Seq.mapi(fun i s -> createStep i s background )
            |> Seq.toList
        
        (background,Expr.NewArray(typeof<Step>,steps))

    
    let createFeature providerName (path:string) =
        let document = parser.Parse(path)
        let providedAssembly = ProvidedAssembly()
        let featureName = document.Feature.Name
        let featureDesc = document.Feature.Description  

            

        let root = ProvidedTypeDefinition(providedAssembly, ns, providerName, Some typeof<obj>, hideObjectMethods=true, nonNullable=true, isErased=false)
        let featureType = ProvidedTypeDefinition(providedAssembly,ns,featureName, Some typeof<Feature>, hideObjectMethods=true, nonNullable=true, isErased=false)

        let scenarios = ProvidedTypeDefinition(providedAssembly, ns, "Scenarios", Some typeof<obj>, isErased=false,hideObjectMethods=true, nonNullable=true)
        let scenarioOutlines = ProvidedTypeDefinition(providedAssembly, ns, "ScenarioOutlines", Some typeof<obj>, isErased=false, hideObjectMethods=true, nonNullable=true)
        
        let createExampleType (gherkinScenario:Ast.Scenario) =
            let examplesName = "Example"
            let header = (gherkinScenario.Examples |> Seq.toList).[0].TableHeader.Cells |> Seq.map (fun c ->c.Value) |> Seq.toList
            createDynamicObject examplesName header typeof<DataCell> createDataInstance

        let createExampleInstances (scenarioOutline:Ast.Scenario) (exampleType:ProvidedTypeDefinition) = 
            let ctr = exampleType.GetConstructors().[0]

            let  examples =
                scenarioOutline.Examples
                |> Seq.collect(fun e -> e.TableBody)
                |> createExamplesFromRows ctr
            
            Expr.NewArray(exampleType,examples)
        
        let children = 
            document.Feature.Children
            |> Seq.choose(
                fun c ->
                    match c with
                    | :? Ast.Background ->
                            let gherkinBackground = c :?> Ast.Background
                            let (backGroundType,steps) = createBackgroundType gherkinBackground
                            let background = Expr.NewObject(Constructors.Background,[Expr.Value(gherkinBackground.Name);Expr.Value(gherkinBackground.Description);steps])
                            backGroundType |> featureType.AddMember
                            ProvidedProperty("Background",backGroundType.AsType(),isStatic = false, getterCode = fun _ -> background )  |> featureType.AddMember
                            ("background",background) |> Some

                    | :? Ast.Scenario ->
                            let gherkinScenario = c :?> Ast.Scenario
                            let scenarioName = gherkinScenario.Name

                            if (gherkinScenario.Examples |> Seq.isEmpty)
                            then
                                let (scenarioType,steps) = createScenarioType gherkinScenario

                                match createTags gherkinScenario.Tags with
                                | None -> ()
                                | Some tags -> tags |> scenarioType.AddMember

                                let scenarioInstance = Expr.NewObject(Constructors.Scenario,[Expr.Value(gherkinScenario.Name);Expr.Value(gherkinScenario.Description);steps;Expr.NewArray(typeof<DataRow>,[])])
                                scenarioType |> scenarios.AddMember
                                ProvidedProperty(scenarioName,scenarioType.AsType(),isStatic = false, getterCode=fun _ -> scenarioInstance) |> scenarios.AddMember
                                ("scenario",scenarioInstance) |> Some
                            else
                                let (scenarioOutlineType,steps) = createScenarioType gherkinScenario

                                match createTags gherkinScenario.Tags with
                                | None -> ()
                                | Some tags -> tags |> scenarioOutlineType.AddMember

                                
                                let exampleType = createExampleType gherkinScenario
                                let examplesType = (typedefof<seq<_>>).MakeGenericType(exampleType.AsType())
                                let examples = createExampleInstances gherkinScenario exampleType
                                let examplesProp = ProvidedProperty("Examples",examplesType,getterCode=fun _ -> examples )

                                exampleType |> scenarioOutlineType.AddMember
                                examplesProp |> scenarioOutlineType.AddMember
                                scenarioOutlineType  |> scenarioOutlines.AddMember

                                let dataRows = 
                                    Expr.NewArray(typeof<DataRow>,[])

                                let scenarioOutlineInstance = Expr.NewObject(Constructors.Scenario,[Expr.Value(gherkinScenario.Name);Expr.Value(gherkinScenario.Description);steps;dataRows])
                                ProvidedProperty(scenarioName,scenarioOutlineType.AsType(),isStatic = false, getterCode=fun _ -> scenarioOutlineInstance) |> scenarioOutlines.AddMember
                                ("scenarioOutline",scenarioOutlineInstance) |> Some
                                
                    | _ -> None
            )
            |> Seq.toList

        let backgroundInstance = 
            match children |> List.tryPick (fun (nm,instance) -> if nm = "background" then Some instance else None) with
            | None -> Expr.Value(null)
            | Some bg -> bg

        let scenarioInstances = Expr.NewArray(typeof<Scenario>, (children |> List.choose (fun (nm,instance) -> if nm = "scenario" then Some instance else None)))
        let scenarioOutlineInstances = Expr.NewArray(typeof<Scenario>, (children |> List.choose (fun (nm,instance) -> if nm = "scenarioOutline" then Some instance else None)))

        scenarios |> featureType.AddMember
        scenarioOutlines |> featureType.AddMember

        ProvidedProperty("Scenarios",scenarios.AsType(),isStatic = false, getterCode = fun _ -> <@@ obj() @@> )  |> featureType.AddMember
        ProvidedProperty("ScenarioOutlines",scenarioOutlines.AsType(),isStatic = false, getterCode = fun _ -> <@@ obj() @@> )  |> featureType.AddMember

        match createTags document.Feature.Tags with
        | None -> ()
        | Some tags -> tags |> featureType.AddMember

        let featureInstance = Expr.NewObject(Constructors.Feature,[Expr.Value(featureName);Expr.Value(featureDesc);scenarioInstances;scenarioOutlineInstances;backgroundInstance])
        let featureProp = ProvidedProperty(featureName,featureType.AsType(),isStatic=true,getterCode=fun _ -> featureInstance) 
        
        featureType |> root.AddMember
        featureProp |> root.AddMember

        providedAssembly.AddTypes [root]

        root
    do
        let gherkinProvider = ProvidedTypeDefinition(asm, ns, "GherkinProvider", None, hideObjectMethods=true, nonNullable=true, isErased=false)
        
        gherkinProvider.DefineStaticParameters( 
                [ProvidedStaticParameter("Feature path", typeof<string>)], 
                fun providerName args -> createFeature providerName (unbox<string> args.[0]))
        this.AddNamespace(ns,[gherkinProvider])
        
[<assembly:CompilerServices.TypeProviderAssembly()>]
do()