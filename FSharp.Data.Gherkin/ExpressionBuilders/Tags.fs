module ExpressionBuilders.Tags

open ExpressionBuilders.Global
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

let sanitizeTagName (nm:string) = nm.Replace("@","") |> SanitizeName

let createTagsType (parent:ProvidedTypeDefinition) (tags:string list) = 
    if tags.Length = 0 then None
    else
        let tagsType = ProvidedTypeDefinition("Tags",Some typeof<obj>,isErased = false)
        
        tagsType |> parent.AddMember

        let parameters = tags |> List.map(fun t -> ProvidedParameter(t |> sanitizeTagName,typeof<string>))
        let fields = tags |> List.map(fun t -> ProvidedField(sprintf "_%s" t |> sanitizeTagName,typeof<string>))
        let properties = tags |> List.mapi(fun i t -> ProvidedProperty(t |> sanitizeTagName,typeof<string>,getterCode = fun args -> Expr.FieldGet(args.[0],fields.[i])))

        fields |> Seq.iter(tagsType.AddMember)
        properties |> Seq.iter(tagsType.AddMember)

        ProvidedConstructor(
            parameters,
            invokeCode =
                fun args ->
                    let this = args.Head
                    let tags = args.Tail
                    let fieldSets = tags |> List.mapi(fun i t -> Expr.FieldSet(this,fields.[i],t))
                    fieldSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) fieldSets.Head
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