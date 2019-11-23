namespace ProviderImplementation

open FSharp.Quotations
open FSharp.Core.CompilerServices
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Reflection

open Gherkin

type Feature (featureName:string,featureDescription:string) =
    member this.FeatureName = featureName
    member this.FeatureDescription = featureDescription

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "FSharp.Data.Gherkin"
    let providerNs = sprintf "%s.Provider" ns
    let featuresNs = sprintf "%s.Features" ns
    let scenariosNs = sprintf "%s.Scenarios" ns
    let backgroundNs = sprintf "%s.Background" ns
    let exampleNs = sprintf "%s.Example" ns
    let stepsNs = sprintf "%s.Steps" ns
    let parser = Parser()
    let asm = Assembly.GetExecutingAssembly()


    let createStep (gherkinStep:Ast.Step) (order:int) (stepName:string)=
        let providedAssembly = ProvidedAssembly()
        let stepText = gherkinStep.Text
        let stepKeyword = gherkinStep.Keyword
        let step = ProvidedTypeDefinition(providedAssembly, stepsNs, stepName, Some typeof<obj>, isErased=false)

        ProvidedProperty("StepText",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepText @@> ) |> step.AddMember
        ProvidedProperty("StepKeyword",typeof<string>,isStatic = false, getterCode = fun _ -> <@@ stepKeyword @@> ) |> step.AddMember
        ProvidedProperty("Order",typeof<int>,isStatic = false, getterCode = fun _ -> <@@ order @@> ) |> step.AddMember

        step
    
    let createScenario (gherkinScenario:Ast.Scenario) =
        let providedAssembly = ProvidedAssembly()
        let scenarioName = gherkinScenario.Name
        let scenarioDesc = gherkinScenario.Description

        let scenario = ProvidedTypeDefinition(providedAssembly, scenariosNs, scenarioName, Some typeof<obj>, isErased=false)

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


        scenario

    
    let createBackground (gherkinBackground:Ast.Background) =
        let providedAssembly = ProvidedAssembly()
        let backgroundName = gherkinBackground.Name
        let backgroundDesc = gherkinBackground.Description

        let background = ProvidedTypeDefinition(providedAssembly, backgroundNs, "Background", Some typeof<obj>, isErased=false)
        
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
        

        let root = ProvidedTypeDefinition(providedAssembly, ns, providerName, Some typeof<obj>, hideObjectMethods=true, nonNullable=true, isErased=false)
        let featureType = ProvidedTypeDefinition(providedAssembly,ns,featureName, Some typeof<Feature>, hideObjectMethods=true, nonNullable=true, isErased=false)

        let scenarios = ProvidedTypeDefinition(providedAssembly, scenariosNs, "Scenarios", Some typeof<obj>, isErased=false)
        
        document.Feature.Children
        |> Seq.iter(
            fun c ->
                match c with
                | :? Ast.Background ->
                        let gherkinBackground = c :?> Ast.Background
                        let background = createBackground gherkinBackground 
                        background |> featureType.AddMember
                        ProvidedProperty("Background",background.AsType(),isStatic = false, getterCode = fun _ -> <@@ obj() @@> )  |> featureType.AddMember
                        
                | :? Ast.Scenario ->
                        let gherkinScenario = c :?> Ast.Scenario
                        let scenarioName = gherkinScenario.Name

                        if (gherkinScenario.Examples |> Seq.isEmpty)
                        then

                            let scenario = createScenario gherkinScenario

                            scenario |> scenarios.AddMember
                        
                            ProvidedProperty(scenarioName,scenario.AsType(),isStatic = false, getterCode=fun _ -> <@@obj()@@>) |> scenarios.AddMember
                        else
                            ()
                | _ -> ()
        )

        scenarios |> featureType.AddMember

        ProvidedProperty("Scenarios",scenarios.AsType(),isStatic = false, getterCode = fun _ -> <@@ obj() @@> )  |> featureType.AddMember


        let featureBaseCtr = typeof<Feature>.GetConstructors().[0]

        let valueField = ProvidedField("_examples",typeof<System.String[]>)
        valueField |> featureType.AddMember

        let examplesProp = ProvidedProperty("Examples",typeof<System.String[]>,isStatic=false,
                                                getterCode= fun args -> Expr.FieldGet(args.[0],valueField))

        examplesProp |> featureType.AddMember

        let featureCtr = 
            ProvidedConstructor(
                [ProvidedParameter("name", typeof<string>);ProvidedParameter("desc", typeof<string>);ProvidedParameter("examples",typeof<System.String[]>)],
                invokeCode = 
                    fun args -> 
                            match args with
                            | [this;_;_;examples] -> Expr.FieldSet(this,valueField,examples)
                            | _ -> failwithf "invalid constructor args"
                    )

        featureCtr.BaseConstructorCall <- 
            fun args -> 
                match args with
                | [this;name;desc;_] -> featureBaseCtr, [this;name;desc]
                | _ -> failwithf "invalid constructor args"

        featureCtr |> featureType.AddMember

        let examples = Expr.NewArray(typeof<System.String>,[Expr.Value("hello");Expr.Value("world")])
        let featureProp = ProvidedProperty(featureName,featureType.AsType(),isStatic=true,getterCode=fun _ -> Expr.NewObject(featureCtr,[Expr.Value(featureName);Expr.Value(featureDesc);examples]))

        
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