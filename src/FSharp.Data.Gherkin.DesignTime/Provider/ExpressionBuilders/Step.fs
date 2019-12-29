module ExpressionBuilders.Step

open ObjectModel
open ExpressionBuilders.Data
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast
open Shared

type StepExpressionBuilder 
        (stepBaseType:System.Type,
        docStringArgumentType:ProvidedTypeDefinition,
        argumentBaseType:System.Type,
        dataExpressionBuilder:DataExpressionBuilder) =

    member __.CreateExpression (parent:ProvidedTypeDefinition) (position:int) (gherkinStep:Step)  =
        let stepName = (sprintf "%i%sClass" position gherkinStep.Text) |> Sanitizer().Sanitize
        let stepType = ProvidedTypeDefinition( stepName,Some stepBaseType,isErased=false, hideObjectMethods=true,isSealed=false)
        stepType |> parent.AddMember
       
        let argumentType =
            if isNull gherkinStep.Argument then None
            else
                match gherkinStep.Argument with
                | :? DocString -> Some (DocStringType docStringArgumentType)
                | :? DataTable -> 
                    let dataTable = gherkinStep.Argument :?> DataTable
                    let columnNames = (dataTable.Rows |> Seq.head).Cells |> Seq.toList |> List.map (fun c -> c.Value)
                    let dataTableRowType = dataExpressionBuilder.CreateExpression stepType columnNames
                    
                    Some (DataTableType (dataTableRowType))
                | _ -> None

        let argumentBackingField = 
            match argumentType with
            | Some argType ->
                let visitedProperty = argumentBaseType.GetProperty("Visited")
                let (argumentField,argumentProperty) =
                    match argType with
                    | DocStringType docStringType ->
                        let argumentField = ProvidedField("_argument",docStringType)

                        argumentField,
                        ProvidedProperty(
                                "Argument",docStringType,
                                getterCode = fun args -> 
                                    let argField = Expr.FieldGet(args.[0],argumentField)
                                    Expr.Sequential(
                                       Expr.PropertySet(argField,visitedProperty,Expr.Value(true)),
                                       Expr.FieldGet(args.[0],argumentField)))

                    | DataTableType (dataTableType) ->
                        let arrayType = dataTableType.MakeArrayType()
                        let argumentField = ProvidedField("_argument",arrayType)

                        argumentField,
                        ProvidedProperty(
                            "Argument",arrayType,
                            getterCode = fun args -> Expr.FieldGet(args.[0],argumentField))
                
                argumentField |> stepType.AddMember
                argumentProperty |> stepType.AddMember        

                Some argumentField
            | _ -> None

        let parameters = 
            let staticParameters =
                [
                    ProvidedParameter("order",typeof<int>)
                    ProvidedParameter("keyword",typeof<string>)
                    ProvidedParameter("text",typeof<string>)
                ]
            match argumentType with
            | None -> ProvidedParameter("argument",argumentBaseType)
            | Some (argType) ->
                    match argType with
                    | DocStringType docStringType -> ProvidedParameter("argument",docStringType)
                    | DataTableType (dataTableType) -> 
                        ProvidedParameter("argument",dataTableType.MakeArrayType())
            :: staticParameters |> List.rev

        let stepCtr =
            ProvidedConstructor(
                parameters,
                invokeCode =
                    fun args ->
                        match argumentBackingField with
                        | None -> <@@ () @@>
                        | Some arg -> 
                            
                            Expr.FieldSet(args.[0],arg,args.[4]))
        
        let baseCtr = stepBaseType.GetConstructors().[0]
        stepCtr.BaseConstructorCall <- 
            fun args -> 
                match argumentType with
                | None ->  baseCtr,[args.[0];args.[1];args.[2];args.[3];Expr.Value(null);Expr.Value(null)]
                | Some argType ->
                    match argType with
                    | DocStringType _ -> baseCtr,[args.[0];args.[1];args.[2];args.[3];args.[4];Expr.Value(null)]
                    | DataTableType _ -> baseCtr,[args.[0];args.[1];args.[2];args.[3];Expr.Value(null);args.[4]]

        stepCtr |> stepType.AddMember

        {
            Name = gherkinStep.Text
            Type = stepType
            Position = position
            Argument = argumentType 
        }
        
    static member CreateNew (providerModel:GherkinProviderModel) (propertyNameSanitizer:string->string) =

        StepExpressionBuilder(
                            providerModel.StepBaseType,
                            providerModel.DocStringArgType,
                            providerModel.ArgumentBaseType,
                            DataExpressionBuilder.CreateNew providerModel propertyNameSanitizer)     


