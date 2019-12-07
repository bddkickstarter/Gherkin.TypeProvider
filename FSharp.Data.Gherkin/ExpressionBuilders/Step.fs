module ExpressionBuilders.Step

open ExpressionBuilders
open ExpressionBuilders.Shared
open ExpressionBuilders.Data

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open System
open Gherkin.Ast

type Argument =
| DocString of ProvidedTypeDefinition
| DataTable of ProvidedTypeDefinition*Type

let createStepExpression  (parent:ProvidedTypeDefinition) (position:int)  (gherkinStep:Step) =

    let stepName = (sprintf "%i %s" position gherkinStep.Text) |> SanitizeName
    let stepType = ProvidedTypeDefinition(stepName,Some (StepBaseType.Value.AsType()),isErased=false)
    stepType |> parent.AddMember
   
    let argumentType =
        if isNull gherkinStep.Argument then None
        else
            match gherkinStep.Argument with
            | :? DocString -> Some (DocString DocStringArgumentType.Value)
            | :? DataTable -> 
                let dataTable = gherkinStep.Argument :?> DataTable
                let columnNames = (dataTable.Rows |> Seq.head).Cells |> Seq.toList |> List.map (fun c -> c.Value)
                let dataTableRowType = createDataExpression stepType columnNames
                
                Some (DataTable (dataTableRowType,dataTableRowType.MakeArrayType()))
            | _ -> None

    let argumentBackingField = 
        match argumentType with
        | Some argType ->
            let (argumentField,argumentProperty) =
                match argType with
                | DocString docStringType ->
                    let argumentField = ProvidedField("_argument",docStringType)

                    argumentField,
                    ProvidedProperty("DocString",docStringType,getterCode = fun args -> Expr.FieldGet(args.[0],argumentField))

                | DataTable (_,arrayType) ->
                    let argumentField = ProvidedField("_argument",arrayType)

                    argumentField,
                    ProvidedProperty("DataTable",arrayType,getterCode = fun args -> Expr.FieldGet(args.[0],argumentField))
            
            argumentField |> stepType.AddMember
            argumentProperty |> stepType.AddMember        

            Some argumentField
        | _ -> None

    let parameters = 
        match argumentType with
        | None ->
            [
                ProvidedParameter("text",typeof<string>)
                ProvidedParameter("keyword",typeof<string>)
                ProvidedParameter("order",typeof<int>)
                ProvidedParameter("order",ArgumentBaseType.Value)
            ]
        | Some (argType) ->
            let argParameter =
                match argType with
                | DocString docStringType -> ProvidedParameter("argument",docStringType)
                | DataTable (_,dataTableType) -> ProvidedParameter("argument",dataTableType)
            [
                ProvidedParameter("text",typeof<string>)
                ProvidedParameter("keyword",typeof<string>)
                ProvidedParameter("order",typeof<int>)
                argParameter
            ]

    let baseCtr = StepBaseType.Value.GetConstructors().[0]
    let stepCtr =
        ProvidedConstructor(
            parameters,
            invokeCode =
                fun args ->
                    match argumentBackingField with
                    | None -> <@@ () @@>
                    | Some arg -> Expr.FieldSet(args.[0],arg,args.[4])
        )
    stepCtr.BaseConstructorCall <- fun args -> baseCtr,[args.[0];args.[1];args.[2];args.[3];args.[4]]
    stepCtr |> stepType.AddMember

    {
        Name = gherkinStep.Text
        Type = stepType
        Position = position
        Argument = match argumentType with 
                   | None -> None 
                   | Some argType ->
                        match argType with 
                        | DocString t -> Some t
                        | DataTable (t,_) -> Some t
    }
