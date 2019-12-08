namespace ExpressionBuilders

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast

type ArgumentExpression =
| DocStringType of ProvidedTypeDefinition
| DataTableType of ProvidedTypeDefinition


type StepExpression = 
    {
        Name:string
        Type:ProvidedTypeDefinition
        Position:int
        Argument:ArgumentExpression option
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

module Shared =

    let mutable SanitizeName :string->string= id

    let mutable TagBaseType :ProvidedTypeDefinition option= None
    let mutable ArgumentBaseType :ProvidedTypeDefinition option= None
    let mutable DataCellType :ProvidedTypeDefinition option= None
    let mutable DataRowBaseType :ProvidedTypeDefinition option= None
    let mutable StepBaseType :ProvidedTypeDefinition option= None
    let mutable ScenarioBaseType :ProvidedTypeDefinition option= None
    let mutable DocStringArgumentType :ProvidedTypeDefinition option= None

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

    let createTagBase (parentName:string) (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_TagBase" parentName |> SanitizeName 
        let tagBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false)
        tagBase |> parent.AddMember

        let nameField = ProvidedField("_name",typeof<string>)
        let nameProperty= ProvidedProperty("Name",typeof<string>,getterCode=fun args -> Expr.FieldGet(args.[0],nameField))

        nameField |> tagBase.AddMember
        nameProperty |> tagBase.AddMember

        let visitedField = addVisitedProperty tagBase

        ProvidedConstructor(
            [ProvidedParameter("name",typeof<string>)],
            invokeCode = 
                fun args -> 
                    Expr.Sequential(
                        Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                        Expr.FieldSet(args.[0],nameField,args.[1])
                    )) |> tagBase.AddMember

        tagBase


    let createArgumentBaseType (parentName:string) (parent:ProvidedTypeDefinition) = 
        let baseName = sprintf "%s_ArgumentBase" parentName |> SanitizeName 
        let docArgumentBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false)
        docArgumentBase |> parent.AddMember

        let visitedField = addVisitedProperty docArgumentBase

        ProvidedConstructor([],invokeCode = fun args -> Expr.FieldSet(args.[0],visitedField,Expr.Value(false))) 
        |> docArgumentBase.AddMember

        docArgumentBase

    let createDataCellType (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DataCell" parentName |> SanitizeName 
            let dataCellType = ProvidedTypeDefinition(baseName,Some (ArgumentBaseType.Value.AsType()),isErased=false)
            dataCellType |> parent.AddMember

            let headerField = ProvidedField("_header",typeof<string>)
            let valueField = ProvidedField("_value",typeof<string>)

            let headerProperty = 
                ProvidedProperty(
                    "Header",typeof<string>,
                    getterCode=fun args -> Expr.FieldGet(args.[0],headerField))

            let valueProperty = 
                ProvidedProperty(
                    "Value",typeof<string>,
                    getterCode=fun args -> Expr.FieldGet(args.[0],valueField))

            headerField |> dataCellType.AddMember
            valueField |> dataCellType.AddMember
            headerProperty |> dataCellType.AddMember
            valueProperty |> dataCellType.AddMember

            let visitedField = addVisitedProperty dataCellType

            ProvidedConstructor(
                [ProvidedParameter("header",typeof<string>);ProvidedParameter("value",typeof<string>)],
                invokeCode =
                    fun args ->
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                            Expr.Sequential(
                                Expr.FieldSet(args.[0],headerField,args.[1]),
                                Expr.FieldSet(args.[0],valueField,args.[2])
                            ))

            ) |> dataCellType.AddMember

            dataCellType

    let createDataRowBaseType (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DataRowBase" parentName |> SanitizeName 
            let dataRowBaseType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false)
            dataRowBaseType |> parent.AddMember

            let cellsArrayType = DataCellType.Value.MakeArrayType()
            let cellsField = ProvidedField("_cells",cellsArrayType)
            let cellsProperty =ProvidedProperty("Cells",cellsArrayType,getterCode = fun args -> Expr.FieldGet(args.[0],cellsField))

            cellsField |> dataRowBaseType.AddMember
            cellsProperty |> dataRowBaseType.AddMember
            
            ProvidedConstructor([ProvidedParameter("cells",cellsArrayType)],
                invokeCode = fun args -> Expr.FieldSet(args.[0],cellsField,args.[1]))
             |> dataRowBaseType.AddMember

            dataRowBaseType

    let createDocStringArgumentType   (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DocString" parentName |> SanitizeName 
            let docArgument = ProvidedTypeDefinition(baseName,Some (ArgumentBaseType.Value.AsType()),isErased=false)
            docArgument |> parent.AddMember

            let contentField = ProvidedField("_content",typeof<string>)
            let contentTypeField = ProvidedField("_contentType",typeof<string>)

            let contentProperty = 
                ProvidedProperty(
                    "Content",typeof<string>,
                    getterCode=fun args -> Expr.FieldGet(args.[0],contentField))

            let contentTypeProperty = 
                ProvidedProperty(
                    "ContentType",typeof<string>,
                    getterCode=fun args -> Expr.FieldGet(args.[0],contentTypeField))

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
                            Expr.FieldSet(args.[0],contentField,args.[1]),
                            Expr.FieldSet(args.[0],contentTypeField,args.[2]))
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

        let docStringField = ProvidedField("_docString",ArgumentBaseType.Value)
        let docStringProperty = ProvidedProperty("DocString",ArgumentBaseType.Value,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],docStringField))

        let dataTableType = DataRowBaseType.Value.MakeArrayType()
        let dataTableField = ProvidedField("_dataTable",dataTableType)
        let dataTableProperty = ProvidedProperty("DataTable",dataTableType,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],dataTableField))


        textField |> step.AddMember
        textProperty |> step.AddMember
        keywordField |> step.AddMember
        keywordProperty |> step.AddMember
        orderField |> step.AddMember
        orderProperty |> step.AddMember
        docStringField |> step.AddMember
        docStringProperty |> step.AddMember
        dataTableField |> step.AddMember
        dataTableProperty |> step.AddMember

        let visitedField = addVisitedProperty step

        ProvidedConstructor(
            [ProvidedParameter("text",typeof<string>);ProvidedParameter("keyword",typeof<string>);ProvidedParameter("order",typeof<int>);ProvidedParameter("docString",ArgumentBaseType.Value);ProvidedParameter("dataTable",DataRowBaseType.Value.MakeArrayType())],
            invokeCode = fun args ->
                let fieldsets =
                    [
                        Expr.FieldSet(args.[0],textField,args.[1])
                        Expr.FieldSet(args.[0],keywordField,args.[2])
                        Expr.FieldSet(args.[0],orderField,args.[3])
                        Expr.FieldSet(args.[0],docStringField,args.[4])
                        Expr.FieldSet(args.[0],dataTableField,args.[5])
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


