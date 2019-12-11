namespace ExpressionBuilders

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Gherkin.Ast

type ArgumentExpression =
| DocStringType of ProvidedTypeDefinition
| DataTableType of ProvidedTypeDefinition

type GeneratedTypeContext =
    {
        SanitizeName :string->string
        TagBaseType :ProvidedTypeDefinition
        ArgumentBaseType :ProvidedTypeDefinition
        DataCellType :ProvidedTypeDefinition
        DataRowBaseType :ProvidedTypeDefinition
        StepBaseType :ProvidedTypeDefinition
        ScenarioBaseType :ProvidedTypeDefinition
        DocStringArgumentType :ProvidedTypeDefinition
    }

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
        let fieldName = sprintf "_%s" (name.ToLower())
        let field = ProvidedField(fieldName,propertyType)
        let property = 
            ProvidedProperty(
                name,propertyType,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],field)))

        field |> parent.AddMember
        property |> parent.AddMember

        field

    let getStepName sanitizeName (position:int) (step:Step) =
        sprintf "%i %s %s" position (step.Keyword.Trim()) (step.Text.Trim())
        |> sanitizeName

    let createTagBase sanitizeName (parentName:string) (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_TagBase" parentName |> sanitizeName 
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

    let createArgumentBaseType sanitizeName (parentName:string) (parent:ProvidedTypeDefinition) = 
        let baseName = sprintf "%s_ArgumentBase" parentName |> sanitizeName 
        let docArgumentBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
        docArgumentBase |> parent.AddMember

        let visitedField = addVisitedProperty docArgumentBase

        ProvidedConstructor([],invokeCode = fun args -> Expr.FieldSet(args.[0],visitedField,Expr.Value(false))) 
        |> docArgumentBase.AddMember

        docArgumentBase

    let createDataCellType sanitizeName (argumentBaseType:ProvidedTypeDefinition) (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DataCell" parentName |> sanitizeName 
            let dataCellType = ProvidedTypeDefinition(baseName,Some (argumentBaseType.AsType()),isErased=false, hideObjectMethods=true)
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

    let createDataRowBaseType sanitizeName (dataCellType:ProvidedTypeDefinition) (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DataRowBase" parentName |> sanitizeName 
            let dataRowBaseType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            dataRowBaseType |> parent.AddMember

            let cellsArrayType = dataCellType.MakeArrayType()
            let cellsField = addProperty dataRowBaseType "Cells" cellsArrayType
            
            ProvidedConstructor([ProvidedParameter("cells",cellsArrayType)],
                invokeCode = fun args -> Expr.FieldSet(args.[0],cellsField,args.[1]))
             |> dataRowBaseType.AddMember

            dataRowBaseType

    let createDocStringArgumentType sanitizeName (argumentBaseType:ProvidedTypeDefinition) (parentName:string) (parent:ProvidedTypeDefinition) =
            let baseName = sprintf "%s_DocString" parentName |> sanitizeName 
            let docArgument = ProvidedTypeDefinition(baseName,Some (argumentBaseType.AsType()),isErased=false, hideObjectMethods=true)
            docArgument |> parent.AddMember

            let contentField = addProperty docArgument "Content" typeof<string> 
            let contentTypeField = addProperty docArgument "ContentType" typeof<string>

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
    
    let createStepBaseType sanitizeName (argumentBaseType:ProvidedTypeDefinition) (dataRowBaseType:ProvidedTypeDefinition) (parentName:string)  (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_StepBase" parentName |> sanitizeName  
        let step = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        step |> parent.AddMember

        let textField = addProperty step "Text" typeof<string>
        let keywordField =addProperty step "Keyword" typeof<string> 
        let orderField = addProperty step "Order" typeof<int>  
        let docStringField = addProperty step "DocString" argumentBaseType  
        let dataTableType = dataRowBaseType.MakeArrayType()
        let dataTableField =  addProperty step "DataTable" dataTableType  
        let visitedField = addVisitedProperty step

        ProvidedConstructor(
            [ProvidedParameter("text",typeof<string>);ProvidedParameter("keyword",typeof<string>);ProvidedParameter("order",typeof<int>);ProvidedParameter("docString",argumentBaseType);ProvidedParameter("dataTable",dataRowBaseType.MakeArrayType())],
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

    let createScenarioBaseType sanitizeName (parentName:string) (parent:ProvidedTypeDefinition) =
        let baseName = sprintf "%s_ScenarioBase" parentName |> sanitizeName  
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


    let createContext (providerName:string) (root:ProvidedTypeDefinition) (shouldSantize:bool)=

        let nameSanitizer = if shouldSantize then sanitize else id
        let argumentBase = createArgumentBaseType nameSanitizer providerName root
        let dataCellType = createDataCellType nameSanitizer argumentBase providerName root
        let dataRowBase = createDataRowBaseType nameSanitizer dataCellType providerName root

        {
            SanitizeName = nameSanitizer
            TagBaseType = createTagBase nameSanitizer providerName root
            ArgumentBaseType = argumentBase
            DataCellType = dataCellType
            DataRowBaseType = dataRowBase
            DocStringArgumentType = createDocStringArgumentType nameSanitizer argumentBase providerName root
            StepBaseType = createStepBaseType nameSanitizer argumentBase dataRowBase providerName root
            ScenarioBaseType = createScenarioBaseType nameSanitizer providerName root
        }


