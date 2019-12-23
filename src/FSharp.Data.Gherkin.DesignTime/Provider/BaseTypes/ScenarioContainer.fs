namespace BaseTypes.ScenarioContainer

open ProviderImplementation.ProvidedTypes
open Shared
open BaseTypes.Scenario
open FSharp.Quotations

type ScenarioContainerBase (scenarioBase:ScenarioBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
        let baseName = sprintf "%s_ScenarioContainerBase" parentName |> Sanitizer().Sanitize  
        let scenarioContainerBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        scenarioContainerBase |> parent.AddMember
        
        // untyped array of scenario base
        let scenariosType = scenarioBase.Type.MakeArrayType();
        let scenariosField = PropertyHelper(scenarioContainerBase).AddProperty("All",scenariosType)
                
        ProvidedConstructor(
                   [ProvidedParameter("scenariosArray",scenariosType)],
                   invokeCode =
                        fun args ->
                            Expr.FieldSet(args.[0],scenariosField,args.[1])
                   ) |> scenarioContainerBase.AddMember
        
        scenarioContainerBase
        
    member val Type = baseType with get
        
    

