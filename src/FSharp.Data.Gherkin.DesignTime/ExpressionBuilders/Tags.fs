module ExpressionBuilders.Tags

open ExpressionBuilders.Shared
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations



let createTagsType (context:GeneratedTypeContext) (parent:ProvidedTypeDefinition) (tags:string list) = 
    if tags.Length = 0 then None
    else
        let tagsType = ProvidedTypeDefinition("TagsClass",Some typeof<obj>,isErased = false, hideObjectMethods=true, isSealed=false)
        
        tagsType |> parent.AddMember

        let sanitizeTagName (nm:string) = nm.Replace("@","") |> context.SanitizeName

        //create params, fields & named properties for all the tags
        let parameters = tags |> List.map(fun t -> ProvidedParameter(t |> sanitizeTagName,typeof<string>))
        let fields = tags |> List.map(fun t -> ProvidedField(sprintf "_%s" t |> sanitizeTagName,context.TagBaseType))

        let visitedTagProperty = context.TagBaseType.GetProperty("Visited")
        let properties = 
            List.map2(
                fun tag field -> 
                    ProvidedProperty(
                        tag |> sanitizeTagName,context.TagBaseType,
                        getterCode = 
                            fun args -> 
                                let tagField = Expr.FieldGet(args.[0],field)
                                let setVisited = Expr.PropertySet(tagField,visitedTagProperty,Expr.Value(true))
                                Expr.Sequential(setVisited,tagField)
                            )) tags fields

        fields |> Seq.iter(tagsType.AddMember)
        properties |> Seq.iter(tagsType.AddMember)

        //add the all tags array
        let allTagsField = addProperty tagsType "AllTags" (context.TagBaseType.MakeArrayType())        

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
                                let tag = Expr.NewObject(context.TagBaseType.GetConstructors().[0],[tagName])
                                Expr.FieldSet(this,field,tag)) tags fields

                    //create array with newly created tags
                    let tatgFields = 
                        fields
                        |> List.map(fun field -> Expr.FieldGet(this,field))

                    let allTagsArray = Expr.FieldSet(this,allTagsField,Expr.NewArray(context.TagBaseType,tatgFields))

                    let setAllFields = fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head

                    // set fields before building the array
                    Expr.Sequential(setAllFields,allTagsArray)
                    )
         |> tagsType.AddMember

        Some tagsType

let createTagsExpression (context:GeneratedTypeContext) (parent:ProvidedTypeDefinition) (tags:string list) =
    match createTagsType context parent tags  with
    | None -> None
    | Some tagType ->
        let tagField = addProperty parent "Tags" tagType
        Some (tagType,tagField)