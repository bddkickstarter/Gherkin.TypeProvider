namespace BaseTypes.RuleContainer

open ProviderImplementation.ProvidedTypes
open Shared
open BaseTypes.Rule
open FSharp.Quotations

type RuleContainerBase (ruleBase:RuleBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
        let baseName = sprintf "%s_RuleContainerBase" parentName |> Sanitizer().Sanitize  
        let rulesContainerBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        rulesContainerBase |> parent.AddMember
        
        // untyped array of rules base
        let propertyHelper = PropertyHelper(rulesContainerBase)
        let rulesType = ruleBase.Type.MakeArrayType();
        let rulesField = propertyHelper.AddProperty("All",rulesType)
                
        ProvidedConstructor(
                   [ProvidedParameter("rulesArray",rulesType)],
                   invokeCode =
                        fun args ->
                            Expr.FieldSet(args.[0],rulesField,args.[1])
                   ) |> rulesContainerBase.AddMember
        
        rulesContainerBase
        
    member val Type = baseType with get