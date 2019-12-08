module ExpressionBuilders.Tags

open ExpressionBuilders.Shared
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

let sanitizeTagName (nm:string) = nm.Replace("@","") |> SanitizeName

let createTagsType (parent:ProvidedTypeDefinition) (tags:string list) = 
    if tags.Length = 0 then None
    else
        let tagsType = ProvidedTypeDefinition("TagsClass",Some typeof<obj>,isErased = false)
        
        tagsType |> parent.AddMember

        //create params, fields & named properties for all the tags
        let parameters = tags |> List.map(fun t -> ProvidedParameter(t |> sanitizeTagName,typeof<string>))
        let fields = tags |> List.map(fun t -> ProvidedField(sprintf "_%s" t |> sanitizeTagName,TagBaseType.Value))

        let visitedTagProperty = TagBaseType.Value.GetProperty("Visited")
        let properties = 
            List.map2(
                fun tag field -> 
                    ProvidedProperty(
                        tag |> sanitizeTagName,TagBaseType.Value,
                        getterCode = 
                            fun args -> 
                                let tagField = Expr.FieldGet(args.[0],field)
                                let setVisited = Expr.PropertySet(tagField,visitedTagProperty,Expr.Value(true))
                                Expr.Sequential(setVisited,tagField)
                            )) tags fields

        fields |> Seq.iter(tagsType.AddMember)
        properties |> Seq.iter(tagsType.AddMember)

        //add the all tags array
        let allTagsType = TagBaseType.Value.MakeArrayType()
        let allTagsField = ProvidedField("_allTags",allTagsType)
        let allTagsProperty = ProvidedProperty("AllTags",allTagsType,getterCode=fun args -> Expr.FieldGet(args.[0],allTagsField))

        allTagsField |> tagsType.AddMember
        allTagsProperty |> tagsType.AddMember

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
                                let tag = Expr.NewObject(TagBaseType.Value.GetConstructors().[0],[tagName])
                                Expr.FieldSet(this,field,tag)) tags fields

                    //create array with newly created tags
                    let tatgFields = 
                        fields
                        |> List.map(fun field -> Expr.FieldGet(this,field))

                    let allTagsArray = Expr.FieldSet(this,allTagsField,Expr.NewArray(TagBaseType.Value,tatgFields))

                    let setAllFields = fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head

                    // set fields before building the array
                    Expr.Sequential(setAllFields,allTagsArray)
                    )
         |> tagsType.AddMember

        Some tagsType

let createTagsExpression (parent:ProvidedTypeDefinition) (tags:string list) =
    match createTagsType parent tags  with
    | None -> None
    | Some tagType ->
        let tagField = ProvidedField("_tags",tagType)
        let tagProperty = ProvidedProperty("Tags",tagType,getterCode = fun args -> Expr.FieldGet(args.[0],tagField))

        tagField |> parent.AddMember
        tagProperty |> parent.AddMember

        Some (tagType,tagField)