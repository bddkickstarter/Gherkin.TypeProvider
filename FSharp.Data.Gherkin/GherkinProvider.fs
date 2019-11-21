namespace ProviderImplementation

open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open ProviderImplementation.ProvidedTypes

open Gherkin

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "FSharp.Data.Gherkin"
    let providerNs = sprintf "%s.Provider" ns
    let featuresNs = sprintf "%s.Features" ns
    let scenariosNs = sprintf "%s.Scenarios" ns
    let backgroundNs = sprintf "%s.Background" ns
    let stepsNs = sprintf "%s.Steps" ns
    let parser = Parser()
    let asm = Assembly.GetExecutingAssembly()



    // let createExampleTable (scenarioName:string) (examples:seq<Ast.Examples>)  (scenarioType:ProvidedTypeDefinition)=
    //     let asm = ProvidedAssembly()
    //     let exampleType =ProvidedTypeDefinition(asm,ns,(sprintf "%s.GeneratedExample" scenarioName) , Some typeof<obj>, isErased=false, isSealed=true, nonNullable = true, hideObjectMethods = true, isInterface = false) 
        
    //     let valueField = ProvidedField("_value", typeof<string>)
    //     ProvidedProperty("column 1", typeof<string>,isStatic=false,getterCode= fun _ -> Expr.FieldGet(valueField)) |> exampleType.AddMember

        
    //     let constructor = ProvidedConstructor( [ProvidedParameter("value", typeof<string>)], 
    //                             invokeCode = 
    //                                 fun args ->
    //                                     match args with
    //                                     | [this;value] ->
    //                                       Expr.FieldSet (this, valueField, <@@ %%value:string @@>)
    //                                     | _ -> 
    //                                         failwith "wrong ctor params")
        
    //     let exampleListType =ProvidedTypeDefinition(asm, ns,(sprintf "%s.GeneratedExamples" scenarioName) , Some typeof<list<_>>, isErased=false,hideObjectMethods = true, nonNullable = true) 
    //     exampleListType |> scenarioType.AddMember

    //     let expressions =[Expr.NewObject(constructor,[Expr.Value("foo")])] 


    //     ProvidedProperty("Examples",typeof<string list>,isStatic = false, getterCode=fun _ ->  <@@ ["test"]  @@>) |> scenarioType.AddMember


    // let createExampleTable (examples:seq<Ast.Examples>) (scenarioName:string)=
    //     let providedAssembly = ProvidedAssembly()
    //     let alExamples = examples |> Seq.collect(fun e -> e.TableBody)
    //     let header = (examples |> Seq.item 0).TableHeader.Cells

    //     ProvidedTypeDefinition(providedAssembly, ns, (sprintf "%s.Examples" scenarioName) , Some typeof<seq<obj>>, isErased=false, hideObjectMethods = true, nonNullable = true)

        // header |> Seq.iteri(
        //     fun i h -> ProvidedProperty(h.Value,typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepText @@> ) |> examples.AddMember
        // )
        
        // ProvidedProperty("StepKeyword",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepKeyword @@> ) |> examples.AddMember
        
    let createExampleTableInstances (value:string) (scenario:ProvidedTypeDefinition) = 
        let providedAsm = ProvidedAssembly()
        let exampleType =ProvidedTypeDefinition(providedAsm,ns,"Example" , Some typeof<obj>, isErased=false, isSealed=true, nonNullable = true, hideObjectMethods = true, isInterface = false) 
        
        // let exampleConstructor = 
        //     ProvidedConstructor(
        //         [ProvidedParameter("prop1", typeof<string>)],
        //         invokeCode = 
        //                 fun args ->
        //                     match args with
        //                     | [this;value] ->
        //                         let valueField = ProvidedProperty("Prop1", typeof<string>)
        //                         valueField |> exampleType.AddMember
        //                         Expr.PropertySet (this, valueField, <@@ %%value:string @@>)
        //                     | _ -> 
        //                         failwith "wrong ctor params")
        
        // exampleConstructor |> exampleType.AddMember
        // let examples = Expr.NewArray ((exampleType.AsType()),[Expr.NewObject(exampleConstructor,[Expr.Value(value)])])


        exampleType
        

    let createStep (gherkinStep:Ast.Step) (order:int) (stepName:string)=
        let providedAssembly = ProvidedAssembly()
        let stepText = gherkinStep.Text
        let stepKeyword = gherkinStep.Keyword
        let step = ProvidedTypeDefinition(providedAssembly, stepsNs, stepName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)

        ProvidedProperty("StepText",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepText @@> ) |> step.AddMember
        ProvidedProperty("StepKeyword",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepKeyword @@> ) |> step.AddMember
        ProvidedProperty("Order",typeof<int>,isStatic = false, getterCode = fun _ -> <@@ order @@> ) |> step.AddMember

        step
    
    let createScenario (gherkinScenario:Ast.Scenario) =
        let providedAssembly = ProvidedAssembly()
        let scenarioName = gherkinScenario.Name
        let scenarioDesc = gherkinScenario.Description

        let scenario = ProvidedTypeDefinition(providedAssembly, scenariosNs, scenarioName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)

        gherkinScenario.Steps
        |> Seq.iteri(
            fun i s ->
                let stepName = sprintf "%i. %s%s" i s.Keyword s.Text
                let step = createStep s i stepName
                step |> scenario.AddMember 

                ProvidedProperty(stepName,step.AsType(),isStatic = false, getterCode=fun _ -> <@@ obj() @@>) |> scenario.AddMember

        )

        ProvidedProperty("ScenarioName",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ scenarioName @@> ) |> scenario.AddMember
        ProvidedProperty("ScenarioDescription",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ scenarioDesc @@> ) |> scenario.AddMember

        let exampleType = createExampleTableInstances "Foo" scenario
        let t = (typedefof<list<_>>).MakeGenericType(exampleType.AsType())
        let getterCode _ = <@@ [] @@> 
        let setterCode _ = <@@ () @@> 
        

        exampleType |> scenario.AddMember
        ProvidedProperty("Examples",t,isStatic=false,getterCode=getterCode,setterCode=setterCode) |> scenario.AddMember

        scenario

    
    let createBackground (gherkinBackground:Ast.Background) =
        let providedAssembly = ProvidedAssembly()
        let backgroundName = gherkinBackground.Name
        let backgroundDesc = gherkinBackground.Description

        let background = ProvidedTypeDefinition(providedAssembly, backgroundNs, "Background", Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)
        
        gherkinBackground.Steps
        |> Seq.iteri(
            fun i s ->
                let stepName = sprintf "%i. %s%s" i s.Keyword s.Text
                let step = createStep s i s.Text
                step |> background.AddMember 

                ProvidedProperty(stepName,step.AsType(),isStatic = false, getterCode=fun _ -> <@@ obj() @@>) |> background.AddMember

        )
        
        ProvidedProperty("BackgroundName",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ backgroundName @@> ) |> background.AddMember
        ProvidedProperty("BackgroundDescription",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ backgroundDesc @@> ) |> background.AddMember
        
        background

    
    let createFeature providerName (path:string) =
        let document = parser.Parse(path)
        let providedAssembly = ProvidedAssembly()
        let featureName = document.Feature.Name
        let featureDesc = document.Feature.Description
        

        let feature = ProvidedTypeDefinition(providedAssembly, featuresNs, providerName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)
        ProvidedProperty("FeatureName",typeof<string>,isStatic = true, getterCode = fun _ -> <@@ featureName @@> ) |> feature.AddMember
        ProvidedProperty("FeatureDescription",typeof<string>,isStatic = true, getterCode = fun _ -> <@@ featureDesc @@> ) |> feature.AddMember
        
        
        let scenarios = ProvidedTypeDefinition(providedAssembly, scenariosNs, "Scenarios", Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)
        
        document.Feature.Children
        |> Seq.iter(
            fun c ->
                match c with
                | :? Ast.Background ->
                        let gherkinBackground = c :?> Ast.Background
                        let background = createBackground gherkinBackground 
                        background |> feature.AddMember
                        ProvidedProperty("Background",background.AsType(),isStatic = true, getterCode = fun _ -> <@@ obj() @@> )  |> feature.AddMember
                        
                | :? Ast.Scenario ->
                        let gherkinScenario = c :?> Ast.Scenario
                        let scenarioName = gherkinScenario.Name
                        let scenario = createScenario gherkinScenario

                        scenario |> scenarios.AddMember
                        
                        ProvidedProperty(scenarioName,scenario.AsType(),isStatic = false, getterCode=fun _ -> <@@ obj() @@>) |> scenarios.AddMember
                | _ -> ()
        )

        scenarios |> feature.AddMember
        ProvidedProperty("Scenarios",scenarios.AsType(),isStatic = true, getterCode = fun _ -> <@@ obj() @@> )  |> feature.AddMember
        

        providedAssembly.AddTypes [feature]

        feature

    do
        let gherkinProvider = ProvidedTypeDefinition(asm, providerNs, "GherkinProvider", None, isErased=false, hideObjectMethods = true, nonNullable = true)
        gherkinProvider.DefineStaticParameters( 
                [ProvidedStaticParameter("Feature path", typeof<string>)], 
                fun providerName args -> createFeature providerName (unbox<string> args.[0]))
        this.AddNamespace(ns,[gherkinProvider])
        

[<assembly:CompilerServices.TypeProviderAssembly()>]
do()