namespace ObjectModel

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

type Sanitizer(?sanitizeType:string) =
    let numbers = [48..57]
    let lowercaseChars = [97..122]
    let uppercaseChars = [65..90]
    let allowedFSharpCharacters = [32]

    let sanitizeFirstNumber  (str:string) =
            match (str.ToCharArray() |> Seq.toList) with
            | n :: _ when System.Char.IsNumber(n) -> sprintf "_%s" str
            | _ -> str


    let allowCharacters  (validCharacters:int list)  (str:string) =
            str.ToCharArray() 
            |> Seq.map(
                fun c -> 
                    let asciiCode = (c |> int) 
                    if validCharacters |> Seq.exists(fun a -> a = asciiCode) then c
                    else '_'
                    )
            |> Seq.toArray

    let sanitizeByType (nm:string) =
        match sanitizeType with
        | None | Some "c#" ->
            nm 
            |> sanitizeFirstNumber
            |> allowCharacters (numbers @ lowercaseChars @ uppercaseChars)
            |> System.String
        | Some "f#" -> 
            nm 
            |> allowCharacters (allowedFSharpCharacters @ numbers @ lowercaseChars @ uppercaseChars)
            |> System.String
        | _ -> nm

    member __.Sanitize(str:string) = sanitizeByType str

type PropertyHelper  (parent:ProvidedTypeDefinition) =

    member __.AddProperty (name:string,propertyType:System.Type) =
        let fieldName = (sprintf "_%s" (name.ToLower())) |> Sanitizer().Sanitize
        let field = ProvidedField(fieldName,propertyType)
        let property = 
            ProvidedProperty(
                name,propertyType,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],field)))

        field |> parent.AddMember
        property |> parent.AddMember

        field

    member __.AddVisitedProperty() =
        let visitedField = ProvidedField("_visited",typeof<bool>)
        let visitedProperty = 
            ProvidedProperty(
                "Visited",typeof<bool>,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],visitedField)),
                setterCode = (fun args -> Expr.FieldSet(args.[0],visitedField,args.[1])))

        visitedField |> parent.AddMember
        visitedProperty |> parent.AddMember

        visitedField 

type TagBase (parentName:string,parentType:ProvidedTypeDefinition) =
    let baseType =
            let baseName = sprintf "%s_TagBase" parentName |> Sanitizer().Sanitize
            let tagBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            tagBase |> parentType.AddMember

            let propertyHelper = PropertyHelper(tagBase)
            let nameField = propertyHelper.AddProperty("Name",typeof<string>)
            let visitedField = propertyHelper.AddVisitedProperty()

            ProvidedConstructor(
                [ProvidedParameter("name",typeof<string>)],
                invokeCode = 
                    fun args -> 
                        Expr.Sequential(
                            Expr.FieldSet(args.[0],visitedField,Expr.Value(false)),
                            Expr.FieldSet(args.[0],nameField,args.[1])
                        )) |> tagBase.AddMember

            tagBase

    member val Type = baseType with get 

type ArgumentBase (parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
        let baseName = sprintf "%s_ArgumentBase" parentName |> Sanitizer().Sanitize 
        let docArgumentBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
        docArgumentBase |> parent.AddMember

        let visitedField = PropertyHelper(docArgumentBase).AddVisitedProperty()

        ProvidedConstructor([],invokeCode = fun args -> Expr.FieldSet(args.[0],visitedField,Expr.Value(false))) 
        |> docArgumentBase.AddMember

        docArgumentBase
    
    member val Type = baseType with get 

type DataCellBase (argumentBase:ArgumentBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
            let baseName = sprintf "%s_DataCell" parentName |> Sanitizer().Sanitize 
            let dataCellType = ProvidedTypeDefinition(baseName,Some (argumentBase.Type.AsType()),isErased=false, hideObjectMethods=true)
            dataCellType |> parent.AddMember

            let propertyHelper = PropertyHelper(dataCellType)

            let headerField = propertyHelper.AddProperty("Header",typeof<string>)
            let valueField = propertyHelper.AddProperty("Value",typeof<string>)
            let visitedField = propertyHelper.AddVisitedProperty()

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

    member val Type = baseType with get

type DataRowBase (dataCellBase:DataCellBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
            let baseName = sprintf "%s_DataRowBase" parentName |> Sanitizer().Sanitize 
            let dataRowBaseType = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
            dataRowBaseType |> parent.AddMember

            let cellsArrayType = dataCellBase.Type.MakeArrayType()
            let cellsField = PropertyHelper(dataRowBaseType).AddProperty("Cells",cellsArrayType)
            
            ProvidedConstructor([ProvidedParameter("cells",cellsArrayType)],
                invokeCode = fun args -> Expr.FieldSet(args.[0],cellsField,args.[1]))
             |> dataRowBaseType.AddMember

            dataRowBaseType
    
    member val Type = baseType with get

type DocStringArgumentBase (argumentBase:ArgumentBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
            let baseName = sprintf "%s_DocString" parentName |> Sanitizer().Sanitize 
            let docArgument = ProvidedTypeDefinition(baseName,Some (argumentBase.Type.AsType()),isErased=false, hideObjectMethods=true)
            docArgument |> parent.AddMember

            let propertyHelper = PropertyHelper(docArgument)
            let contentField = propertyHelper.AddProperty("Content",typeof<string> )
            let contentTypeField = propertyHelper.AddProperty("ContentType",typeof<string>)

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

    member val Type = baseType with get

type StepBase (argumentBase:ArgumentBase,dataRowBase:DataRowBase,parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
        let baseName = sprintf "%s_StepBase" parentName |> Sanitizer().Sanitize  
        let step = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        step |> parent.AddMember

        let propertyHelper = PropertyHelper(step)

        let textField = propertyHelper.AddProperty("Text",typeof<string>)
        let keywordField =propertyHelper.AddProperty("Keyword",typeof<string>)
        let orderField = propertyHelper.AddProperty("Order",typeof<int>)
        let docStringField = propertyHelper.AddProperty("DocString",argumentBase.Type)
        let dataTableType = dataRowBase.Type.MakeArrayType()
        let dataTableField =  propertyHelper.AddProperty("DataTable",dataTableType)
        let visitedField = propertyHelper.AddVisitedProperty()

        ProvidedConstructor(
            [ProvidedParameter("text",typeof<string>);ProvidedParameter("keyword",typeof<string>);ProvidedParameter("order",typeof<int>);ProvidedParameter("docString",argumentBase.Type);ProvidedParameter("dataTable",dataRowBase.Type.MakeArrayType())],
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

    member val Type = baseType with get

    static member GetStepName (sanitizeName:string->string,position:int,step:Step) =
        sprintf "%i %s %s" position (step.Keyword.Trim()) (step.Text.Trim())
        |> sanitizeName

type ScenarioBase (stepBase:StepBase,parentName:string,parent:ProvidedTypeDefinition) =
    let baseType =
        let baseName = sprintf "%s_ScenarioBase" parentName |> Sanitizer().Sanitize  
        let scenarioBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false,isSealed=false, hideObjectMethods=true)
        scenarioBase |> parent.AddMember

        let propertyHelper = PropertyHelper(scenarioBase)
        let nameField = propertyHelper.AddProperty("Name",typeof<string>)
        let descriptionField =  propertyHelper.AddProperty("Description",typeof<string>)
        let stepsType = stepBase.Type.AsType().MakeArrayType()
        let stepsField = propertyHelper.AddProperty("Steps",stepsType)
        let visitedField = propertyHelper.AddVisitedProperty()

        ProvidedConstructor(
            [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);ProvidedParameter("steps",stepsType)],
            invokeCode = 
                fun args ->
                    [
                        Expr.FieldSet(args.[0],nameField,args.[1])
                        Expr.FieldSet(args.[0],descriptionField,args.[2])
                        Expr.FieldSet(args.[0],stepsField,args.[3])
                    ]
                    |> Seq.fold(fun a c -> Expr.Sequential(a,c)) (Expr.FieldSet(args.[0],visitedField,Expr.Value(false)))
                   
                )|> scenarioBase.AddMember

        scenarioBase
    
    member val Type = baseType with get

type FeatureBase (providerName:string,root:ProvidedTypeDefinition,sanitizeType:string) =
    let context = 
        let argumentBase = ArgumentBase(providerName,root)
        let dataCellType = DataCellBase(argumentBase,providerName,root)
        let dataRowBase = DataRowBase(dataCellType,providerName,root)
        let tagBase = TagBase(providerName,root)
        let docStringBase = DocStringArgumentBase(argumentBase,providerName,root)
        let stepBase = StepBase(argumentBase,dataRowBase,providerName,root)
        let scenarioBase = ScenarioBase(stepBase,providerName,root)

        {
            SanitizeName = Sanitizer(sanitizeType).Sanitize
            TagBaseType = tagBase.Type
            ArgumentBaseType = argumentBase.Type
            DataCellType = dataCellType.Type
            DataRowBaseType = dataRowBase.Type
            DocStringArgumentType = docStringBase.Type
            StepBaseType = stepBase.Type
            ScenarioBaseType = scenarioBase.Type
        }

    member __.GetContext() = context

    


    
    


