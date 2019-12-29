namespace ProviderImplementation

open BaseTypes.Argument
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection
open Gherkin
open ExpressionBuilders.Feature
open ExpressionBuilders.Scenario
open ExpressionBuilders.Step
open Shared
open InstanceBuilders.Feature
open ObjectModel
open FSharp.Quotations
open InstanceBuilders.Feature

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config,assemblyReplacementMap=[("FSharp.Data.Gherkin.DesignTime", "FSharp.Data.Gherkin")],addDefaultProbingLocation=true)

    let ns = "FSharp.Data.Gherkin"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<GherkinProvider.Runtime.AssemblyChecker>.Assembly.GetName().Name = asm.GetName().Name)  

    let create providerName (path:string) (sanitizetype:string) =
        if not (["none";"partial";"full"] |> Seq.exists(fun s -> sanitizetype = s))
            then failwith ("Invalid Sanitize type. Must be: none, partial or full")
        else
            let providedAssembly = ProvidedAssembly()
            let root = ProvidedTypeDefinition(providedAssembly,ns,providerName,Some typeof<obj>,isErased=false)
            let gherkinDocument = Parser().Parse(path)
            
            let providerModel = GherkinProviderModel(providerName,root)
            let expressionBuilder = FeatureExpressionBuilder.CreateNew providerModel (Sanitizer(sanitizetype).Sanitize)
            let instanceBuilder= FeatureBuilder.CreateNewFeature providerModel
            
            let sbCtr = providerModel.ScenarioBaseType.GetConstructors().[0]
            let sbInstance = Expr.NewObject(sbCtr,
                                            [
                                                Expr.Coerce(Expr.Value(null),providerModel.ScenarioBaseType)
                                                Expr.Value("")
                                                Expr.Value("")
                                                Expr.Coerce(Expr.Value(null),providerModel.TagContainerBaseType)
                                                Expr.Coerce(Expr.Value(null), providerModel.DataRowBaseType.MakeArrayType())
                                                Expr.Coerce(Expr.Value(null),providerModel.StepBaseType.MakeArrayType())
                                            ])
            
            let stepCtr = providerModel.StepBaseType.GetConstructors().[0]
            let stepInstance = Expr.NewObject(stepCtr,
                                              [
                                                  Expr.Value("")
                                                  Expr.Value("")
                                                  Expr.Value(0)
                                                  Expr.Coerce(Expr.Value(null),providerModel.ArgumentBaseType)
                                                  Expr.Coerce(Expr.Value(null),providerModel.DataRowBaseType.MakeArrayType())
                                              ])
            
            let baseClasses = Expr.NewTuple([sbInstance;stepInstance])
            
            ProvidedProperty("BaseClasses",baseClasses.Type,isStatic=true,getterCode=fun _ -> baseClasses) |> root.AddMember

            expressionBuilder.CreateExpression providerName root gherkinDocument
            |> instanceBuilder.BuildFeature root gherkinDocument
            
            providedAssembly.AddTypes [root]
            root

    do
        let provider = ProvidedTypeDefinition(asm, ns, "GherkinProvider", None, isErased=false)

        provider.DefineStaticParameters( 
                [
                    ProvidedStaticParameter("FeaturePath", typeof<string>)
                    ProvidedStaticParameter("Sanitize", typeof<string>,"none")
                ], 
                fun providerName args -> 
                    create providerName (unbox<string> args.[0]) (unbox<string> args.[1])  )
        this.AddNamespace(ns,[provider])
