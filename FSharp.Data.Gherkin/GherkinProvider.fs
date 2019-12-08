namespace Gherkin.ProviderImplementation

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection
open Gherkin
open ExpressionBuilders.Feature
open InstanceBuilders.Feature
open ExpressionBuilders.Shared

[<TypeProvider>]
type GherkinProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "FSharp.Data.Gherkin"
    let asm = Assembly.GetExecutingAssembly()

    let create providerName (path:string) =
        let providedAssembly = ProvidedAssembly()
        let root = ProvidedTypeDefinition(providedAssembly,ns,providerName,Some typeof<obj>,isErased=false)
        let gherkinDocument = Parser().Parse(path)

        createFeatureExpression providerName root gherkinDocument
        |> buildFeatureInstance root gherkinDocument

        providedAssembly.AddTypes [root]
        root

    do
        let provider = ProvidedTypeDefinition(asm, ns, "GherkinProvider", None, isErased=false)

        provider.DefineStaticParameters( 
                [
                    ProvidedStaticParameter("FeaturePath", typeof<string>)
                    ProvidedStaticParameter("Sanitize", typeof<bool>,false)
                ], 
                fun providerName args -> 
                
                    match (unbox<bool> args.[1]) with 
                    | true -> SanitizeName <- ExpressionBuilders.Shared.sanitize
                    | _ -> ()

                    create providerName (unbox<string> args.[0]) )
        this.AddNamespace(ns,[provider])

[<assembly:TypeProviderAssembly()>]
do()

