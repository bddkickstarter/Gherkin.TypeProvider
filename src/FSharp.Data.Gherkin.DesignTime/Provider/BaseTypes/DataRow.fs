namespace BaseTypes.DataRow

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.DataCell

type DataRowBase (dataCellBase:DataCellBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
            let baseName = sprintf "%s_DataRowBase" parentName |> Sanitizer().Sanitize 
            let dataRowBaseType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            dataRowBaseType |> parent.AddMember

            let cellsArrayType = dataCellBase.Type.MakeArrayType()
            let cellsField = PropertyHelper(dataRowBaseType).AddProperty("Cells",cellsArrayType)
            
            ProvidedConstructor([ProvidedParameter("cells",cellsArrayType)],
                invokeCode = fun args -> Expr.FieldSet(args.[0],cellsField,args.[1]))
             |> dataRowBaseType.AddMember

            dataRowBaseType
    
    member val Type = baseType with get