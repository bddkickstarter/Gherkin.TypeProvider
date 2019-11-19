namespace ProviderImplementation

open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open ProviderImplementation.ProvidedTypes

open Gherkin

// type Scenario = 
//     {
//         ScenarioName:string
//         ScenarioDescription:string
//     }

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "FSharp.Data.Gherkin"
    let parser = Parser()
    let asm = Assembly.GetExecutingAssembly()


    // let createSteps (scenarioName:string) (steps:seq<Ast.Step>) (scenarioType:ProvidedTypeDefinition) =
    //     let asm = ProvidedAssembly()
    //     steps
    //     |> Seq.iteri
    //         (
    //             fun i step ->
    //                 let stepName = sprintf "%i. %s%s" i step.Keyword step.Text
    //                 let stepType =ProvidedTypeDefinition(asm, ns,(sprintf "%sGeneratedStep%i" scenarioName i) , Some typeof<Step>, isErased=false, hideObjectMethods = true, nonNullable = true) 
    //                 let stepProperty =ProvidedProperty(stepName, stepType,isStatic=false,getterCode= fun _ -> Expr.Value((step,i),typeof<Step>)) 
    //                 stepType |> scenarioType.AddMember
    //                 stepProperty |> scenarioType.AddMember
    //         )

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
        

    // let createScenario (scenario:Ast.Scenario) (featureType:ProvidedTypeDefinition) = 
    //     let asm = ProvidedAssembly()
    //     let scenarioType =ProvidedTypeDefinition(asm, ns,scenario.Name , Some typeof<Scenario>, isErased=false,hideObjectMethods = true, nonNullable = true) 
    //     scenarioType |> featureType.AddMember

    //     ProvidedProperty(scenario.Name,scenarioType,isStatic = false, getterCode = fun _ -> Expr.Value(scenario, typeof<Scenario>)) |> featureType.AddMember

    //     createExampleTable scenario.Name scenario.Examples scenarioType

    //     createSteps scenario.Name scenario.Steps scenarioType 
            


    // let createScenarios (feature:Ast.Feature) (featureType:ProvidedTypeDefinition) =
    //     let asm = ProvidedAssembly()
    //     feature.Children 
    //     |> Seq.iter
    //         (
    //             fun c -> 
    //                 match c with
    //                 | :? Ast.Scenario -> 
    //                     let scenario =  c :?> Ast.Scenario
    //                     createScenario scenario featureType

    //                 | :? Ast.Background ->
    //                     let background = c :?> Ast.Background

    //                     let backgroundContainerPropertyType = ProvidedTypeDefinition(asm,ns,"Background",Some typeof<Background>, isErased=false, hideObjectMethods = true, nonNullable = true) 
    //                     let backgroundProperty= ProvidedProperty("Background",backgroundContainerPropertyType ,isStatic=false, getterCode = fun _ -> Expr.Value(background, typeof<Background>))  

    //                     backgroundContainerPropertyType |> featureType.AddMember
    //                     backgroundProperty |> featureType.AddMember

    //                     createSteps "BackgroundSteps" background.Steps backgroundContainerPropertyType           

    //                 | _ -> ProvidedTypeDefinition(asm, ns,"Unknown" , Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true) |> featureType.AddMember
    //         )
        
    
    
    // let createFeatureProperty  (document:Ast.GherkinDocument) =
    //     //let asm = ProvidedAssembly()
    //     let featureName = document.Feature.Name
    //     ProvidedProperty(featureName,featureType,isStatic = true, getterCode = fun _ -> Expr.Value(document.Feature, typeof<Feature>)) |> featureType.AddMember
    //     // createScenarios document.Feature featureType


    // let createFeatureProvider providerName (path:string)  =
    //     let asm = ProvidedAssembly()
    //     let dynamicFeatureType = ProvidedTypeDefinition(asm, ns, providerName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)
    //     createFeatureProperty (parser.Parse(path))
    //     dynamicFeatureType

    let createExampleTable (examples:seq<Ast.Examples>) (scenarioName:string)=
        let providedAssembly = ProvidedAssembly()
        let alExamples = examples |> Seq.collect(fun e -> e.TableBody)
        let header = (examples |> Seq.item 0).TableHeader.Cells

        ProvidedTypeDefinition(providedAssembly, ns, (sprintf "%s.Examples" scenarioName) , Some typeof<seq<obj>>, isErased=false, hideObjectMethods = true, nonNullable = true)

        // header |> Seq.iteri(
        //     fun i h -> ProvidedProperty(h.Value,typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepText @@> ) |> examples.AddMember
        // )
        
        // ProvidedProperty("StepKeyword",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepKeyword @@> ) |> examples.AddMember
        


    let createStep (gherkinStep:Ast.Step) (order:int) (stepName:string)=
        let providedAssembly = ProvidedAssembly()
        let stepText = gherkinStep.Text
        let stepKeyword = gherkinStep.Keyword
        let step = ProvidedTypeDefinition(providedAssembly, ns, stepName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)

        ProvidedProperty("StepText",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepText @@> ) |> step.AddMember
        ProvidedProperty("StepKeyword",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepKeyword @@> ) |> step.AddMember
        ProvidedProperty("Order",typeof<int>,isStatic = false, getterCode = fun _ -> <@@ order @@> ) |> step.AddMember
        
        step
    
    let createScenario (gherkinScenario:Ast.Scenario) =
        let providedAssembly = ProvidedAssembly()
        let scenarioName = gherkinScenario.Name
        let scenarioDesc = gherkinScenario.Description

        let scenario = ProvidedTypeDefinition(providedAssembly, ns, scenarioName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)

        gherkinScenario.Steps
        |> Seq.iteri(
            fun i s ->
                let stepName = sprintf "%i. %s%s" i s.Keyword s.Text
                let step = createStep s i s.Text
                step |> scenario.AddMember 

                ProvidedProperty(stepName,step.AsType(),isStatic = false, getterCode=fun _ -> <@@ obj() @@>) |> scenario.AddMember

        )

        ProvidedProperty("ScenarioName",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ scenarioName @@> ) |> scenario.AddMember
        ProvidedProperty("ScenarioDescription",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ scenarioDesc @@> ) |> scenario.AddMember
        
        scenario

    
    let createFeature providerName (path:string) =
        let document = parser.Parse(path)
        let providedAssembly = ProvidedAssembly()
        let featureName = document.Feature.Name
        let featureDesc = document.Feature.Description

        let feature = ProvidedTypeDefinition(providedAssembly, ns, providerName, Some typeof<obj>, isErased=false, hideObjectMethods = true, nonNullable = true)

        document.Feature.Children
        |> Seq.iter(
            fun c ->
                match c with
                | :? Ast.Background ->()
                | :? Ast.Scenario ->
                        let gherkinScenario = c :?> Ast.Scenario
                        let scenarioName = gherkinScenario.Name
                        let scenario = createScenario gherkinScenario

                        scenario |> feature.AddMember
                        
                        ProvidedProperty(scenarioName,scenario.AsType(),isStatic = true, getterCode=fun _ -> <@@ obj() @@>) |> feature.AddMember
                | _ -> ()
        )



        ProvidedProperty("FeatureName",typeof<string>,isStatic = true, getterCode = fun _ -> <@@ featureName @@> ) |> feature.AddMember
        ProvidedProperty("FeatureDescription",typeof<string>,isStatic = true, getterCode = fun _ -> <@@ featureDesc @@> ) |> feature.AddMember
        
        providedAssembly.AddTypes [feature]

        feature

        

    do
        let gherkinProvider = ProvidedTypeDefinition(asm, ns, "GherkinProvider", None, isErased=false, hideObjectMethods = true, nonNullable = true)
        gherkinProvider.DefineStaticParameters( 
                [ProvidedStaticParameter("Feature path", typeof<string>)], 
                fun providerName args -> createFeature providerName (unbox<string> args.[0]))
        this.AddNamespace(ns,[gherkinProvider])
        

[<assembly:CompilerServices.TypeProviderAssembly()>]
do()