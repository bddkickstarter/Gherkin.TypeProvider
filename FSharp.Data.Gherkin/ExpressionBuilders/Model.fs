namespace ExpressionBuilders

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast

type StepExpression = 
    {
        Name:string
        Type:ProvidedTypeDefinition
        Position:int
        Argument:ProvidedTypeDefinition option
    }

type ScenarioExpression =
    {
        Name:string
        Type:ProvidedTypeDefinition
        Steps:StepExpression list
        Examples:ProvidedTypeDefinition option
        Tags:ProvidedTypeDefinition option
    }

type BackgroundExpression =
    {
        Type:ProvidedTypeDefinition
        Steps:StepExpression list
    }


type FeatureExpression =
    {
        Name:string
        Type:ProvidedTypeDefinition
        Scenarios:ScenarioExpression list
        Background:BackgroundExpression option
        Tags:ProvidedTypeDefinition option
    }

module Global =

    let mutable SanitizeName :string->string= id

    let mutable StepBaseType :ProvidedTypeDefinition option= None
    let mutable ScenarioBaseType :ProvidedTypeDefinition option= None
    let mutable DocStringArgumentType :ProvidedTypeDefinition option= None

    

module Shared =
    open Global

    let sanitize  = 
        fun (nm:string) ->
            let sanitizeFirstNumber =
                match (nm.ToCharArray() |> Seq.toList) with
                | n :: _ when System.Char.IsNumber(n) -> sprintf "_%s" nm
                | _ -> nm 

            sanitizeFirstNumber
                .Replace(" ","_")
                .Replace("<","_")
                .Replace(">","_")
                .Replace("@","_")

    let addVisitedProperty (parent:ProvidedTypeDefinition) =
        let visitedField = ProvidedField("_visited",typeof<bool>)
        let visitedProperty = 
            ProvidedProperty(
                "Visited",typeof<bool>,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],visitedField)),
                setterCode = (fun args -> Expr.FieldSet(args.[0],visitedField,args.[1])))

        visitedField |> parent.AddMember
        visitedProperty |> parent.AddMember

        visitedField

    let getStepName (position:int) (step:Step) =
        sprintf "%i %s %s" position (step.Keyword.Trim()) (step.Text.Trim())
        |> SanitizeName




module BaseTypes =
    open Shared
    open Global

    let createDocStringArgumentType (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_Argument" parentName |> SanitizeName 
            let docArgument = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false)
            docArgument |> parent.AddMember

            let visitedField = addVisitedProperty docArgument

            let contentField = ProvidedField("_content",typeof<string>)
            let contentTypeField = ProvidedField("_contentType",typeof<string>)

            let contentProperty = 
                ProvidedProperty(
                    "Content",typeof<string>,
                    getterCode=fun args -> 
                    Expr.Sequential(
                        Expr.FieldSet(args.[0],visitedField,Expr.Value(true)),
                        Expr.FieldGet(args.[0],contentField)))

            let contentTypeProperty = 
                ProvidedProperty(
                    "ContentType",typeof<string>,
                    getterCode=fun args -> 
                    Expr.Sequential(
                        Expr.FieldSet(args.[0],visitedField,Expr.Value(true)),
                        Expr.FieldGet(args.[0],contentTypeField)))

            contentField |> docArgument.AddMember
            contentTypeField |> docArgument.AddMember
            contentProperty |> docArgument.AddMember
            contentTypeProperty |> docArgument.AddMember

            ProvidedConstructor(
                [
                    ProvidedParameter("_content",typeof<string>)
                    ProvidedParameter("_contentType",typeof<string>)
                ],
                invokeCode =
                    fun args ->
                        Expr.Sequential(
                            Expr.Sequential(
                                Expr.FieldSet(args.[0],contentField,args.[1]),
                                Expr.FieldSet(args.[0],contentTypeField,args.[2])
                            ),Expr.FieldSet(args.[0],visitedField,Expr.Value(false)))
            ) |> docArgument.AddMember

            docArgument
    
    let createStepBaseType (parentName:string)  (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_StepBase" parentName |> SanitizeName  
        let step = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false)
        step |> parent.AddMember

        let textField = ProvidedField("_text",typeof<string>)
        let textProperty = ProvidedProperty("Text",typeof<string>,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],textField))

        let keywordField = ProvidedField("_keyword",typeof<string>)
        let keywordProperty = ProvidedProperty("Keyword",typeof<string>,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],keywordField))

        let orderField = ProvidedField("_order",typeof<int>)
        let orderProperty = ProvidedProperty("Order",typeof<int>,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],orderField))

        textField |> step.AddMember
        textProperty |> step.AddMember
        keywordField |> step.AddMember
        keywordProperty |> step.AddMember
        orderField |> step.AddMember
        orderProperty |> step.AddMember

        let visitedField = addVisitedProperty step

        ProvidedConstructor(
            [ProvidedParameter("text",typeof<string>);ProvidedParameter("keyword",typeof<string>);ProvidedParameter("order",typeof<int>)],
            invokeCode = fun args ->
                let fieldsets =
                    [
                        Expr.FieldSet(args.[0],textField,args.[1])
                        Expr.FieldSet(args.[0],keywordField,args.[2])
                        Expr.FieldSet(args.[0],orderField,args.[3])
                        Expr.FieldSet(args.[0],visitedField,Expr.Value(false))
                    ]
                fieldsets.Tail
                |> List.fold (fun a c -> Expr.Sequential(a,c)) fieldsets.Head
                    
                ) |> step.AddMember

        step

    let createScenarioBaseType (parentName:string) (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_ScenarioBase" parentName |> SanitizeName  
        let scenarioBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false)
        scenarioBase |> parent.AddMember

        // name & description fields,props & params
        let nameField = ProvidedField("_name",typeof<string>)
        let descriptionField = ProvidedField("_description",typeof<string>)

        let nameProperty = ProvidedProperty("Name",typeof<string>,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],nameField))
        let descriptionProperty= ProvidedProperty("Description",typeof<string>,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],descriptionField))


        nameField |> scenarioBase.AddMember
        descriptionField |> scenarioBase.AddMember

        nameProperty |> scenarioBase.AddMember
        descriptionProperty |> scenarioBase.AddMember

        let visitedField = addVisitedProperty scenarioBase

        ProvidedConstructor(
            [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);],
            invokeCode = 
                fun args ->
                    Expr.Sequential(
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                            Expr.FieldSet(args.[0],nameField,args.[1])
                        ),Expr.FieldSet(args.[0],descriptionField,args.[2]))
                   
                    )|> scenarioBase.AddMember

        scenarioBase


