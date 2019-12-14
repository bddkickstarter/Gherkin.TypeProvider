
namespace BaseTypes.Step

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.Argument
open BaseTypes.DataRow
open Gherkin.Ast

type StepBase (argumentBase:ArgumentBase,dataRowBase:DataRowBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
        let baseName = sprintf "%s_StepBase" parentName |> Sanitizer().Sanitize  
        let step = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        step |> parent.AddMember

        let propertyHelper = PropertyHelper(step)

        let textField = propertyHelper.AddProperty("Text",typeof<string>)
        let keywordField =propertyHelper.AddProperty("Keyword",typeof<string>)
        let orderField = propertyHelper.AddProperty("Order",typeof<int>)
        let docStringField = propertyHelper.AddProperty("DocString",argumentBase.Type)
        let dataTableType = dataRowBase.Type.MakeArrayType()
        let dataTableField =  propertyHelper.AddProperty("DataTable",dataTableType)
        let visitedField = propertyHelper.AddVisitedProperty()

        ProvidedConstructor([
                ProvidedParameter("text",typeof<string>)
                ProvidedParameter("keyword",typeof<string>)
                ProvidedParameter("order",typeof<int>)
                ProvidedParameter("docString",argumentBase.Type)
                ProvidedParameter("dataTable",dataRowBase.Type.MakeArrayType())],
            invokeCode = fun args ->
                [
                    Expr.FieldSet(args.[0],textField,args.[1])
                    Expr.FieldSet(args.[0],keywordField,args.[2])
                    Expr.FieldSet(args.[0],orderField,args.[3])
                    Expr.FieldSet(args.[0],docStringField,args.[4])
                    Expr.FieldSet(args.[0],dataTableField,args.[5])
                ]
                |> List.fold (fun a c -> Expr.Sequential(a,c)) (Expr.FieldSet(args.[0],visitedField,Expr.Value(false)))
                    
                ) |> step.AddMember

        step

    member val Type = baseType with get

    static member GetStepName (sanitizeName:string->string,position:int,step:Step) =
        sprintf "%i %s %s" position (step.Keyword.Trim()) (step.Text.Trim())
        |> sanitizeName