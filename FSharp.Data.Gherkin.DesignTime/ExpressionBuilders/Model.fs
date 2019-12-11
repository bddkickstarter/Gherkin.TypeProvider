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
            let sanitizeFirstNumber  =
                match (nm.ToCharArray() |> Seq.toList) with
                | n :: _ when System.Char.IsNumber(n) -> sprintf "_%s" nm
                | _ -> nm 

            [' ';'<';'>'] @ (System.IO.Path.GetInvalidPathChars() |> Seq.toList)
            |> Seq.fold (fun (a:string) c -> a.Replace(c.ToString(),"_") ) sanitizeFirstNumber

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

    let addProperty (parent:ProvidedTypeDefinition) (name:string) (propertyType:System.Type)=
        let fieldName = sprintf "_%s" name
        let field = ProvidedField(fieldName,propertyType)
        let property = 
            ProvidedProperty(
                name,propertyType,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],field)))

        field |> parent.AddMember
        property |> parent.AddMember

        field


    let getStepName (position:int) (step:Step) =
        sprintf "%i %s %s" position (step.Keyword.Trim()) (step.Text.Trim())
        |> SanitizeName

    let createTagBase (parentName:string) (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_TagBase" parentName |> SanitizeName 
        let tagBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
        tagBase |> parent.AddMember

        let nameField = addProperty tagBase "Name" typeof<string>
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
        let docArgumentBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
        docArgumentBase |> parent.AddMember

        let visitedField = addVisitedProperty docArgumentBase

        ProvidedConstructor([],invokeCode = fun args -> Expr.FieldSet(args.[0],visitedField,Expr.Value(false))) 
        |> docArgumentBase.AddMember

        docArgumentBase

    let createDataCellType (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DataCell" parentName |> SanitizeName 
            let dataCellType = ProvidedTypeDefinition(baseName,Some (ArgumentBaseType.Value.AsType()),isErased=false, hideObjectMethods=true)
            dataCellType |> parent.AddMember

            let headerField = addProperty dataCellType "Header" typeof<string>
            let valueField = addProperty dataCellType "Value" typeof<string>
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
            let dataRowBaseType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            dataRowBaseType |> parent.AddMember

            let cellsArrayType = DataCellType.Value.MakeArrayType()
            let cellsField = addProperty dataRowBaseType "Cells" cellsArrayType
            
            ProvidedConstructor([ProvidedParameter("cells",cellsArrayType)],
                invokeCode = fun args -> Expr.FieldSet(args.[0],cellsField,args.[1]))
             |> dataRowBaseType.AddMember

            dataRowBaseType

    let createDocStringArgumentType   (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DocString" parentName |> SanitizeName 
            let docArgument = ProvidedTypeDefinition(baseName,Some (ArgumentBaseType.Value.AsType()),isErased=false, hideObjectMethods=true)
            docArgument |> parent.AddMember

            let contentField = addProperty docArgument "Content" typeof<string> 
            let contentTypeField = addProperty docArgument "ContentType" typeof<string>

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
        let step = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        step |> parent.AddMember

        let textField = addProperty step "Text" typeof<string>
        let keywordField =addProperty step "Keyword" typeof<string> 
        let orderField = addProperty step "Order" typeof<int>  
        let docStringField = addProperty step "DocString" ArgumentBaseType.Value  
        let dataTableType = DataRowBaseType.Value.MakeArrayType()
        let dataTableField =  addProperty step "DataTable" dataTableType  
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
        let scenarioBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        scenarioBase |> parent.AddMember

        let nameField = addProperty scenarioBase "Name" typeof<string> 
        let descriptionField =  addProperty scenarioBase "Description" typeof<string> 
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


