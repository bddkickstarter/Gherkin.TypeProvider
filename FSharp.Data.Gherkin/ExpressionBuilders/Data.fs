module ExpressionBuilders.Data

open ExpressionBuilders.Shared
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

let createDataExpression (parent:ProvidedTypeDefinition)  (columnNames:string list) = 
    let dataType  = ProvidedTypeDefinition("Data",Some typeof<obj>, isErased=false)
    dataType |> parent.AddMember

    let visitedField = addVisitedProperty dataType
    
    // create constructor parameters for each of the columns
    let parameters = 
        columnNames
        |> List.map(fun h -> ProvidedParameter(h |> SanitizeName,typeof<string>))
    
    // create fields for each of the columns
    let fields = 
        columnNames 
        |> List.map(fun h -> ProvidedField( sprintf "_%s" (h |> SanitizeName),typeof<string>))

    fields |> Seq.iter (dataType.AddMember)

    // create properties getting the correct backing field
    let properties =
        columnNames
        |> Seq.mapi(
            fun i h -> 
                ProvidedProperty(
                    h |> SanitizeName,
                    typeof<string>,
                    getterCode=
                        fun args ->

                            //get the specific column field
                            let columnField = Expr.FieldGet(args.[0],fields.[i])

                            Expr.Sequential(
                                //visit column
                                Expr.FieldSet(args.[0],visitedField,Expr.Value(true)),
                                //return column
                                columnField
                                )
                         ))

    properties |> Seq.iter (dataType.AddMember)
    
    let ctr =
        ProvidedConstructor(parameters,
            fun args -> 
                match args with
                | this :: xs when xs.Length <> 0 -> 
                    xs 
                    |> List.mapi(fun i paramVal -> Expr.FieldSet(this,fields.[i],paramVal) )
                    |> List.fold (fun a c -> Expr.Sequential(a, c)) (Expr.FieldSet(this,visitedField,Expr.Value(false)))
                | _ -> failwith ("incorrect constructor arguments"))
    
    ctr |> dataType.AddMember


    dataType