namespace BaseTypes.DataCell

open BaseTypes.Argument
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared

type DataCellBase (argumentBase:ArgumentBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
            let baseName = sprintf "%s_DataCell" parentName |> Sanitizer().Sanitize 
            let dataCellType = ProvidedTypeDefinition(baseName,Some (argumentBase.Type.AsType()),isErased=false, hideObjectMethods=true)
            dataCellType |> parent.AddMember

            let propertyHelper = PropertyHelper(dataCellType)

            let headerField = propertyHelper.AddProperty("Header",typeof<string>)
            let valueField = propertyHelper.AddProperty("Value",typeof<string>)
            let visitedField = propertyHelper.AddVisitedProperty()

            ProvidedConstructor(
                [ProvidedParameter("header",typeof<string>);ProvidedParameter("value",typeof<string>)],
                invokeCode =
                    fun args ->
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                            Expr.Sequential(
                                Expr.FieldSet(args.[0],headerField,args.[1]),
                                Expr.FieldSet(args.[0],valueField,args.[2])
                            ))

            ) |> dataCellType.AddMember

            dataCellType

    member val Type = baseType with get