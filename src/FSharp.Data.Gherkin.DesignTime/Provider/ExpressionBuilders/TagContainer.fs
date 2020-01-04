module ExpressionBuilders.TagContainer

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open BaseTypes.Tag
open BaseTypes.TagContainer
open ObjectModel
open Shared

type TagContainerExpressionBuilder (tagBaseType:System.Type,tagContainerBase:ProvidedTypeDefinition) =

    let createTagsType (parent:ProvidedTypeDefinition) (tags:string list) = 
        if tags.Length = 0 then None
        else
            let tagsContainer = ProvidedTypeDefinition("TagsContainer",Some (tagContainerBase.AsType()),isErased = false, hideObjectMethods=true, isSealed=false)
            let sanitizeTagName (nm:string) = nm.Replace("@","") |> Sanitizer().Sanitize
            tagsContainer |> parent.AddMember

            //create params, fields & named properties for all the tags
            let parameters = tags |> List.map(fun t -> ProvidedParameter(t |> sanitizeTagName,tagBaseType))
            let fields = tags |> List.map(fun t -> ProvidedField(sprintf "_%s" t |> sanitizeTagName,tagBaseType))

            let visitedTagProperty = tagBaseType.GetProperty("Visited")

            let properties = 
                List.map2(
                    fun tag field -> 
                        ProvidedProperty(
                            tag |> sanitizeTagName,
                            tagBaseType,
                            getterCode = 
                                fun args -> 
                                    let tagField = Expr.FieldGet(args.[0],field)
                                    let setVisited = Expr.PropertySet(tagField,visitedTagProperty,Expr.Value(true))
                                    Expr.Sequential(setVisited,tagField)
                                )) tags fields

            fields |> Seq.iter(tagsContainer.AddMember)
            properties |> Seq.iter(tagsContainer.AddMember)
            
            let ctr =
                ProvidedConstructor(
                    parameters,
                    invokeCode =
                        fun args ->
                            let this = args.Head
                            let tags = args.Tail

                            //set named properties
                            let fieldSets = 
                                 List.map2(
                                     fun field tag -> 
                                        Expr.FieldSet(this,field,tag)) fields tags

                            
                            fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head)

                 
            let baseCtr = tagContainerBase.GetConstructors().[0] 
           
            ctr.BaseConstructorCall <- fun args -> 
                            let this = args.[0]
                            let tags = args.GetSlice(Some 1,Some(args.Length-1))

                            baseCtr, [this;Expr.NewArray(tagBaseType,tags)]

            ctr |> tagsContainer.AddMember

            Some tagsContainer

    member __.CreateExpression (parent:ProvidedTypeDefinition) (tags:string list) =
        match createTagsType parent tags with
        | None -> None
        | Some tagType ->
            let tagField = PropertyHelper(parent).AddProperty("Tags",tagType)
            Some (tagType,tagField)
            
    member __.CreateDefaultTagContainer (parent:ProvidedTypeDefinition) =
            let emptyTags= Expr.NewArray(tagBaseType,[])
            let emptyTagContainer =Expr.NewObject(tagContainerBase.GetConstructors().[0],[emptyTags])
            let defaultContainer = PropertyHelper(parent).AddProperty("Tags",tagContainerBase)
            defaultContainer,emptyTagContainer
            
            
            
    static member CreateNew (providerModel:GherkinProviderModel) =

        TagContainerExpressionBuilder(
                                    providerModel.TagBaseType , 
                                    providerModel.TagContainerBaseType)