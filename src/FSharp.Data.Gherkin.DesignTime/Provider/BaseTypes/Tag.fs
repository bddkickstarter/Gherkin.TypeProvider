namespace BaseTypes.Tag

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared

type TagBase (parentName:string,parentType:ProvidedTypeDefinition) =

    let baseType =
            let baseName = sprintf "%s_TagBase" parentName |> Sanitizer().Sanitize
            let tagBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            tagBase |> parentType.AddMember

            let propertyHelper = PropertyHelper(tagBase)
            let nameField = propertyHelper.AddProperty("Name",typeof<string>)
            let visitedField = propertyHelper.AddVisitedProperty()

            ProvidedConstructor(
                [ProvidedParameter("name",typeof<string>)],
                invokeCode = 
                    fun args -> 
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                            Expr.FieldSet(args.[0],nameField,args.[1])
                        )) |> tagBase.AddMember

            tagBase

    member val Type = baseType with get 