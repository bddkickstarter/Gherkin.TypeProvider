namespace BaseTypes.Rule

open ProviderImplementation.ProvidedTypes
open Shared
open FSharp.Quotations
open BaseTypes.ScenarioContainer
open Gherkin.Ast

type RuleBase (scenarioContainerBase:ScenarioContainerBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
        let baseName = sprintf "%s_RuleBase" parentName |> Sanitizer().Sanitize  
        let ruleBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        ruleBase |> parent.AddMember
        
        let propertyHelper = PropertyHelper(ruleBase)
        
        let keywordField = propertyHelper.AddProperty("Keyword",typeof<string>)
        let nameField = propertyHelper.AddProperty("Name",typeof<string>)
        let descriptionField = propertyHelper.AddProperty("Description",typeof<string>)
        let visitedField = propertyHelper.AddVisitedProperty()
        
        // untyped array of scenario base
        let examplesType = scenarioContainerBase.Type.MakeArrayType();
        let examplesTypeField = propertyHelper.AddProperty("All",examplesType)
                
        ProvidedConstructor(
                   [
                       ProvidedParameter("keyword",typeof<string>)
                       ProvidedParameter("name",typeof<string>)
                       ProvidedParameter("description",typeof<string>)
                       ProvidedParameter("examplesArray",examplesType)
                   ],
                   invokeCode =
                        fun args ->
                            let fieldSets =
                                [
                                 Expr.FieldSet(args.[0],keywordField,args.[1])
                                 Expr.FieldSet(args.[0],nameField,args.[2])
                                 Expr.FieldSet(args.[0],descriptionField,args.[3])
                                 Expr.FieldSet(args.[0],examplesTypeField,args.[4])
                                 Expr.FieldSet(args.[0],visitedField,Expr.Value(false))
                                 ]
                            fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head
                   ) |> ruleBase.AddMember
        
        ruleBase
        
    member val Type = baseType with get
    
