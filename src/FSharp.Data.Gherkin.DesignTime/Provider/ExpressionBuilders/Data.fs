module ExpressionBuilders.Data

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open ObjectModel
open Shared

let sanitize = Sanitizer().Sanitize

type DataExpressionBuilder  (dataRowBaseType:System.Type,dataCellType:System.Type,sanitizePropertyName:string->string) = 
    member __.CreateExpression (parent:ProvidedTypeDefinition) (columnNames:string list) =
        let dataType  = ProvidedTypeDefinition("DataClass",Some dataRowBaseType, isErased=false, hideObjectMethods=true, isSealed=false)
        dataType |> parent.AddMember

        // create constructor parameters for each of the columns
        let parameters = 
            columnNames
            |> List.map(fun h -> ProvidedParameter(h |> sanitize,dataCellType))
        
        // create fields for each of the columns
        let fields = 
            columnNames 
            |> List.map(fun h -> ProvidedField( sprintf "_%s" (h |> sanitize),dataCellType))

        fields |> Seq.iter (dataType.AddMember)

        // create properties getting the correct backing field
        let visitedProperty = dataCellType.GetProperty("Visited")
        let properties =
            Seq.map2(
                fun columnName field -> 
                    ProvidedProperty(
                        columnName |> sanitizePropertyName,
                        dataCellType,
                        getterCode= 
                            fun args ->
                                //set visited of the field's visited property
                                let dataField = Expr.FieldGet(args.[0],field)
                                let visitField = Expr.PropertySet(dataField,visitedProperty,Expr.Value(true))
                                
                                Expr.Sequential(visitField,dataField)
                                )) columnNames fields

        properties |> Seq.iter (dataType.AddMember)

        let ctr =
            ProvidedConstructor(parameters,
                fun args -> 
                    let this = args.Head
                    let fieldParams = args.Tail
                    let fieldSets = List.map2(fun paramVal field -> Expr.FieldSet(this,field,paramVal) ) fieldParams fields

                    fieldSets.Tail |> List.fold (fun a c -> Expr.Sequential(a, c)) fieldSets.Head)
        
        let baseCtr = dataRowBaseType.GetConstructors().[0]
        ctr.BaseConstructorCall <- fun args -> baseCtr,[args.Head;Expr.NewArray(dataCellType,args.Tail)]
        ctr |> dataType.AddMember

        dataType
        
    static member CreateNew (providerModel:GherkinProviderModel) (propertyNameSanitizer:string->string) =
        DataExpressionBuilder(
                                providerModel.DataRowBaseType,
                                providerModel.DataCellBaseType,
                                propertyNameSanitizer)

    