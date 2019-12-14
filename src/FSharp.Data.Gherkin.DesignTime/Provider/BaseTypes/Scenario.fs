namespace BaseTypes.Scenario

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.Step

type ScenarioBase (stepBase:StepBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
        let baseName = sprintf "%s_ScenarioBase" parentName |> Sanitizer().Sanitize  
        let scenarioBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        scenarioBase |> parent.AddMember

        let propertyHelper = PropertyHelper(scenarioBase)
        let nameField = propertyHelper.AddProperty("Name",typeof<string>)
        let descriptionField =  propertyHelper.AddProperty("Description",typeof<string>)
        let stepsType = stepBase.Type.AsType().MakeArrayType()
        let stepsField = propertyHelper.AddProperty("Steps",stepsType)
        let visitedField = propertyHelper.AddVisitedProperty()

        ProvidedConstructor(
            [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);ProvidedParameter("steps",stepsType)],
            invokeCode = 
                fun args ->
                    [
                        Expr.FieldSet(args.[0],nameField,args.[1])
                        Expr.FieldSet(args.[0],descriptionField,args.[2])
                        Expr.FieldSet(args.[0],stepsField,args.[3])
                    ]
                    |> Seq.fold(fun a c -> Expr.Sequential(a,c)) (Expr.FieldSet(args.[0],visitedField,Expr.Value(false)))
                   
                )|> scenarioBase.AddMember

        scenarioBase
    
    member val Type = baseType with get