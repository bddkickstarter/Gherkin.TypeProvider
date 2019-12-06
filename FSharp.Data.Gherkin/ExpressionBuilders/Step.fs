module ExpressionBuilders.Step

open ExpressionBuilders
open ExpressionBuilders.BaseTypes
open ExpressionBuilders.Global
open ExpressionBuilders.Data

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

open Gherkin.Ast

let createStepExpression  (parent:ProvidedTypeDefinition) (position:int)  (gherkinStep:Step) =

    let stepName = (sprintf "%i %s" position gherkinStep.Text) |> SanitizeName
    let stepType = ProvidedTypeDefinition(stepName,Some (StepBaseType.Value.AsType()),isErased=false)
    stepType |> parent.AddMember
   
    let argumentType =
        if isNull gherkinStep.Argument then None
        else
            match gherkinStep.Argument with
            | :? DocString -> Some (DocStringArgumentType.Value,false)
            | :? DataTable -> 
                let dataTable = gherkinStep.Argument :?> DataTable
                let columnNames = (dataTable.Rows |> Seq.head).Cells |> Seq.toList |> List.map (fun c -> c.Value)
                let dataTableRowType = createDataExpression stepType columnNames
                
                Some (dataTableRowType,true)
            | _ -> None

    let argumentBackingField = 
        match argumentType with
        | Some (argType,isArray) ->
            let argumentType = if isArray then argType.MakeArrayType() else argType.AsType()
            let argumentField = ProvidedField("_argument",argumentType)
            let argumentProperty = ProvidedProperty("Argument",argumentType,getterCode = fun args -> Expr.FieldGet(args.[0],argumentField))

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
            ]
        | Some (argType,isArray) ->
            let argumentType = if isArray then argType.MakeArrayType() else argType.AsType()
            let argumentParameter = ProvidedParameter("argument",argumentType)
            [
                ProvidedParameter("text",typeof<string>)
                ProvidedParameter("keyword",typeof<string>)
                ProvidedParameter("order",typeof<int>)
                argumentParameter
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
    stepCtr.BaseConstructorCall <- fun args -> baseCtr,[args.[0];args.[1];args.[2];args.[3]]
    stepCtr |> stepType.AddMember


    {
        Name = gherkinStep.Text
        Type = stepType
        Position = position
        Argument = match argumentType with | None -> None | Some (argType,_) -> Some argType
    }
