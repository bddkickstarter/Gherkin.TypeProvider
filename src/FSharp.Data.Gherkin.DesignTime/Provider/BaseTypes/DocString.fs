namespace BaseTypes.DocString

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.Argument

type DocStringArgumentBase (argumentBase:ArgumentBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
            let baseName = sprintf "%s_DocString" parentName |> Sanitizer().Sanitize 
            let docArgument = ProvidedTypeDefinition(baseName,Some (argumentBase.Type.AsType()),isErased=false, hideObjectMethods=true)
            docArgument |> parent.AddMember

            let propertyHelper = PropertyHelper(docArgument)
            let contentField = propertyHelper.AddProperty("Content",typeof<string> )
            let contentTypeField = propertyHelper.AddProperty("ContentType",typeof<string>)

            ProvidedConstructor(
                [
                    ProvidedParameter("content",typeof<string>)
                    ProvidedParameter("contentType",typeof<string>)
                ],
                invokeCode =
                    fun args ->
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],contentField,args.[1]),
                            Expr.FieldSet(args.[0],contentTypeField,args.[2]))
            ) |> docArgument.AddMember

            docArgument

    member val Type = baseType with get