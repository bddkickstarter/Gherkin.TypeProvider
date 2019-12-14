module ExpressionBuilders.Tags

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared

type TagsExpressionBuilder (tagBaseType:System.Type) =

    let createTagsType (parent:ProvidedTypeDefinition) (tags:string list) = 
        if tags.Length = 0 then None
        else
            let tagsType = ProvidedTypeDefinition("TagsClass",Some typeof<obj>,isErased = false, hideObjectMethods=true, isSealed=false)
            
            tagsType |> parent.AddMember

            let sanitizeTagName (nm:string) = nm.Replace("@","") |> Sanitizer().Sanitize

            //create params, fields & named properties for all the tags
            let parameters = tags |> List.map(fun t -> ProvidedParameter(t |> sanitizeTagName,typeof<string>))
            let fields = tags |> List.map(fun t -> ProvidedField(sprintf "_%s" t |> sanitizeTagName,tagBaseType))

            let visitedTagProperty = tagBaseType.GetProperty("Visited")
            let properties = 
                List.map2(
                    fun tag field -> 
                        ProvidedProperty(
                            tag |> sanitizeTagName,tagBaseType,
                            getterCode = 
                                fun args -> 
                                    let tagField = Expr.FieldGet(args.[0],field)
                                    let setVisited = Expr.PropertySet(tagField,visitedTagProperty,Expr.Value(true))
                                    Expr.Sequential(setVisited,tagField)
                                )) tags fields

            fields |> Seq.iter(tagsType.AddMember)
            properties |> Seq.iter(tagsType.AddMember)

            //add the all tags array
            let allTagsField = PropertyHelper(tagsType).AddProperty("AllTags",(tagBaseType.MakeArrayType()))

            ProvidedConstructor(
                parameters,
                invokeCode =
                    fun args ->
                        let this = args.Head
                        let tags = args.Tail

                        //create tag objects from string parameters
                        let fieldSets = 
                             List.map2(
                                 fun tagName field -> 
                                    let tag = Expr.NewObject(tagBaseType.GetConstructors().[0],[tagName])
                                    Expr.FieldSet(this,field,tag)) tags fields

                        //create array with newly created tags
                        let tatgFields = 
                            fields
                            |> List.map(fun field -> Expr.FieldGet(this,field))

                        let allTagsArray = Expr.FieldSet(this,allTagsField,Expr.NewArray(tagBaseType,tatgFields))

                        let setAllFields = fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head

                        // set fields before building the array
                        Expr.Sequential(setAllFields,allTagsArray)
                        )
             |> tagsType.AddMember

            Some tagsType

    member __.CreateExpression (parent:ProvidedTypeDefinition) (tags:string list) =
        match createTagsType parent tags with
        | None -> None
        | Some tagType ->
            let tagField = PropertyHelper(parent).AddProperty("Tags",tagType)
            Some (tagType,tagField)
