module ExpressionBuilders.Feature

open ExpressionBuilders
open ExpressionBuilders.BaseTypes
open ExpressionBuilders.Shared
open ExpressionBuilders.Global
open ExpressionBuilders.Scenario
open ExpressionBuilders.Tags
open ExpressionBuilders.Background
open ProviderImplementation.ProvidedTypes
open Gherkin.Ast
open FSharp.Quotations

let getScenariosFromDocument (document:GherkinDocument) =
    let scenarios =
        document.Feature.Children |> Seq.toList
        |> List.choose(
            fun c ->
                match c with
                | :? Scenario -> Some  (c :?> Scenario)
                | _ -> None
            )

    let background = 
        document.Feature.Children
        |> Seq.tryPick(
            fun c ->
                match c with
                | :? Background -> Some  (c :?> Background)
                | _ -> None
        )

    background,scenarios,(document.Feature.Tags |> Seq.toList |> List.map(fun t -> t.Name))

let getBackgroundExpression (feature:ProvidedTypeDefinition) (background:Background option) =
        match background with
        | None -> None
        | Some b -> 
            let backgroundType = createBackgroundExpression feature b
            let backgroundField = ProvidedField("_background",backgroundType.Type)
            let backgroundProperty = ProvidedProperty("Background",backgroundType.Type,getterCode = fun args -> Expr.FieldGet(args.[0],backgroundField))

            backgroundField |> feature.AddMember
            backgroundProperty |> feature.AddMember

            Some (backgroundType,backgroundField)


let createFeatureTypeTree (root:ProvidedTypeDefinition) (children:Background option*Scenario list*string list) =
    let (background,scenarios,tags) = children

    let featureType= ProvidedTypeDefinition("Feature",Some typeof<obj>,isErased=false)

    //Add Feature
    featureType |> root.AddMember

    // add name & description
    let nameField = ProvidedField("_name",typeof<string>)
    let descriptionField = ProvidedField("_description",typeof<string>)

    let nameProperty = ProvidedProperty("Name",typeof<string>,getterCode=fun args -> Expr.FieldGet(args.[0],nameField))
    let descriptionProperty= ProvidedProperty("Description",typeof<string>,getterCode=fun args -> Expr.FieldGet(args.[0],descriptionField))

    nameField |> featureType.AddMember
    descriptionField |> featureType.AddMember

    nameProperty |> featureType.AddMember
    descriptionProperty |> featureType.AddMember

    //get background if it exists
    let backgroundExpression = getBackgroundExpression featureType background

    let scenarioExpressions = scenarios |> List.map(createScenarioExpression featureType)
    let scenarioParameters =  scenarioExpressions |> List.map(fun st -> ProvidedParameter(st.Name |> SanitizeName,st.Type))

    //untyped scenarios array - add the typed scenarios as scenariobase
    let scenariosType = ScenarioBaseType.Value.MakeArrayType()
    let scenariosField = ProvidedField("_scenarios",scenariosType)
    let scenariosProperty = ProvidedProperty("Scenarios",scenariosType,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],scenariosField))

    //add the non-derived Scenarios field & property to the feature
    scenariosField |> featureType.AddMember
    scenariosProperty |> featureType.AddMember

    //create tags
    let tagExpression = (createTagsExpression featureType tags) 

    //add the optional parameters
    let parameters = 
        match backgroundExpression,tagExpression with
        | None,None -> [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>)]
        | Some (backgroundType,_),None -> [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);ProvidedParameter("background",backgroundType.Type)]
        | None,Some(tagType,_) -> [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);ProvidedParameter("tags",tagType)]
        | Some (backgroundType,_),Some(tagType,_) -> [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>);ProvidedParameter("background",backgroundType.Type);ProvidedParameter("tags",tagType)]

    //create individual fields to hold the derived scenarios
    let scenarioFields = scenarioExpressions |> List.mapi(fun i sArg-> ProvidedField((sprintf "_scenario%i" i) |> SanitizeName, sArg.Type))

    //get the visited property of the scenario base
    let visitedProperty = ScenarioBaseType.Value.GetProperty("Visited")

    //properties named after the scenario names, accessing backing fields as typed scenarios
    let scenarioProperties = 
        List.map2(
                fun  (scenarioExpression:ScenarioExpression) (scenarioField:ProvidedField) -> 
                ProvidedProperty(
                    scenarioExpression.Name |> SanitizeName,
                    scenarioExpression.Type,
                    isStatic = false,
                    getterCode = 
                        fun args -> 

                            //get the specific scenario field
                            let scenarioFieldGet = Expr.FieldGet(args.[0],scenarioField)

                            Expr.Sequential(
                                //visit scenario
                                Expr.PropertySet(scenarioFieldGet,visitedProperty,Expr.Value(true)),
                                //return scenario
                                scenarioFieldGet
                                )

                            )) scenarioExpressions scenarioFields

    // add fields & properties to feature
    scenarioFields |> Seq.iter (featureType.AddMember)
    scenarioProperties |> Seq.iter (featureType.AddMember)

    let constructorParams = parameters @ scenarioParameters

    ProvidedConstructor(
        constructorParams,
        invokeCode = (fun args ->
            let this = args.[0]

            //set the name & description fields
            let setName = Expr.FieldSet(this,nameField, args.[1])
            let setDescription = Expr.FieldSet(this,descriptionField,args.[2])

            let setDescriptors = Expr.Sequential(setName,setDescription)

            //get the scenarios from arguments based on whether there are tags and/or background
            let scenarios = 
                match backgroundExpression,tagExpression with
                | None,None -> args.GetSlice(Some 3,Some (args.Length-1))
                | Some(_),None ->args.GetSlice(Some 4,Some (args.Length-1))
                | Some(_),Some(_) ->args.GetSlice(Some 5,Some (args.Length-1))
                | None,Some(_) -> args.GetSlice(Some 4,Some (args.Length-1))

            //coerce the derived scenarios to their base class
            let coercedParams = scenarios |> List.map (fun p -> Expr.Coerce(p,ScenarioBaseType.Value))

            //then add them to the array 
            let baseArray = Expr.NewArray(ScenarioBaseType.Value,coercedParams)

            //set the array with the descriptors
            let first = Expr.Sequential(Expr.FieldSet(this,scenariosField, baseArray),setDescriptors)
            
            //set each parameter to its non-derived backing field
            let scenarioFieldSets = List.map2( fun scenarioField scenarioValue -> Expr.FieldSet(this,scenarioField,scenarioValue))  scenarioFields scenarios

            //create a single expression with all the scenarios sets & the new array
            let scenarioFieldSet = scenarioFieldSets |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) first
            
            // add any background and tags
            let additionalSets =
                match backgroundExpression,tagExpression with
                | None,None -> []
                | Some(_,backgroundField),None -> [Expr.FieldSet(this,backgroundField,args.[3])]
                | Some(_,backgroundField),Some(_,tagField) -> [Expr.FieldSet(this,backgroundField,args.[3]);Expr.FieldSet(this,tagField,args.[4])]
                | None,Some(_,tagField) ->  [Expr.FieldSet(this,tagField,args.[3])]


            additionalSets |> Seq.fold(fun a c -> Expr.Sequential(a,c)) scenarioFieldSet


            )) |> featureType.AddMember

    {
        Name = "Feature"
        Type = featureType
        Scenarios = scenarioExpressions
        Background = match backgroundExpression with | None -> None | Some (backgroundExp,_) -> Some backgroundExp
        Tags = match tagExpression with | None -> None | Some (tagExpr,_) -> Some tagExpr
    }


let createFeatureExpression (root:ProvidedTypeDefinition) (document:GherkinDocument) =
    getScenariosFromDocument document 
    |> createFeatureTypeTree root
