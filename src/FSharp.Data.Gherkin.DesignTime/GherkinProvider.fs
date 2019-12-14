﻿namespace ProviderImplementation

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection
open Gherkin
open ExpressionBuilders.Feature
open InstanceBuilders.Feature
open ObjectModel

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config,assemblyReplacementMap=[("FSharp.Data.Gherkin.DesignTime", "FSharp.Data.Gherkin")],addDefaultProbingLocation=true)

    let ns = "FSharp.Data.Gherkin"
    let asm = Assembly.GetExecutingAssembly()

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<GherkinProvider.Runtime.AssemblyChecker>.Assembly.GetName().Name = asm.GetName().Name)  

    let create providerName (path:string) (sanitizetype:string) =
        let providedAssembly = ProvidedAssembly()
        let root = ProvidedTypeDefinition(providedAssembly,ns,providerName,Some typeof<obj>,isErased=false)
        let gherkinDocument = Parser().Parse(path)
        let context = FeatureBase(providerName,root,sanitizetype).GetContext()
        
        FeatureExpressionBuilder(context,providerName,root,gherkinDocument).Expression
        |> buildFeatureInstance context root gherkinDocument

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
