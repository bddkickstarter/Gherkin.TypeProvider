namespace BaseTypes.TagContainer

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.Tag

type TagContainerBase (tagBase:TagBase,parentName:string,parentType:ProvidedTypeDefinition) =

    let baseType =
            let baseName = sprintf "%s_TagContainerBase" parentName |> Sanitizer().Sanitize
            let tagType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            tagType |> parentType.AddMember

            let propertyHelper = PropertyHelper(tagType)
            let tagsField = propertyHelper.AddProperty("AllTags",tagBase.Type.MakeArrayType())

            ProvidedConstructor(
                [ProvidedParameter("tags",tagBase.Type.MakeArrayType())],
                invokeCode = 
                    fun args -> Expr.FieldSet(args.[0],tagsField,args.[1]))

            |> tagType.AddMember

            tagType

    member val Type = baseType with get 