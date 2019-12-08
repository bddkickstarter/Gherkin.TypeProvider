module ExpressionBuilders.Data

open ExpressionBuilders.Shared
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

let createDataExpression (parent:ProvidedTypeDefinition)  (columnNames:string list) = 
    let dataRowBaseType=DataRowBaseType.Value.AsType()
    let dataType  = ProvidedTypeDefinition("Data",Some dataRowBaseType, isErased=false, hideObjectMethods=true)
    dataType |> parent.AddMember

    // create constructor parameters for each of the columns
    let parameters = 
        columnNames
        |> List.map(fun h -> ProvidedParameter(h |> SanitizeName,DataCellType.Value))
    
    // create fields for each of the columns
    let fields = 
        columnNames 
        |> List.map(fun h -> ProvidedField( sprintf "_%s" (h |> SanitizeName),DataCellType.Value))

    fields |> Seq.iter (dataType.AddMember)

    // create properties getting the correct backing field
    let visitedProperty = DataCellType.Value.GetProperty("Visited")
    let properties =
        Seq.map2(
            fun columnName field -> 
                ProvidedProperty(
                    columnName |> SanitizeName,
                    DataCellType.Value,
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

                fieldSets.Tail |> List.fold (fun a c -> Expr.Sequential(a, c)) fieldSets.Head
                )
    
    let baseCtr = DataRowBaseType.Value.GetConstructors().[0]
    ctr.BaseConstructorCall <- fun args -> baseCtr,[args.Head;Expr.NewArray(DataCellType.Value,args.Tail)]
    ctr |> dataType.AddMember


    dataType