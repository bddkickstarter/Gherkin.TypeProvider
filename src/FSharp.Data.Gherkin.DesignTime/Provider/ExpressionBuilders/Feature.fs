module ExpressionBuilders.Feature

open ObjectModel
open ExpressionBuilders.Data
open ExpressionBuilders.Step
open ExpressionBuilders.Scenario
open ExpressionBuilders.TagContainer
open ExpressionBuilders.Background
open ProviderImplementation.ProvidedTypes
open Gherkin.Ast
open FSharp.Quotations
open Shared

let sanitize = Sanitizer().Sanitize

type FeatureExpressionBuilder  
            (backgroundExpression:BackgroundExpressionBuilder,
            scenarioExpressionBuilder:ScenarioExpressionBuilder,
            scenarioBaseType:System.Type,
            tagContainerExpressionBuilder:TagContainerExpressionBuilder,
            propertyNameSanitizer:string->string) =

    let getFeatureItemsFromDocument (document:GherkinDocument) =
        let scenarios =
            document.Feature.Children |> Seq.toList
            |> List.choose(
                fun c ->
                    match c with
                    | :? Scenario -> Some  (c :?> Scenario)
                    | _ -> None)

        let background = 
            document.Feature.Children
            |> Seq.tryPick(
                fun c ->
                    match c with
                    | :? Background -> Some  (c :?> Background)
                    | _ -> None)

        background,scenarios,(document.Feature.Tags |> Seq.toList |> List.map(fun t -> t.Name))

    let getBackgroundExpression (feature:ProvidedTypeDefinition) (background:Background option) =
            match background with
            | None -> None
            | Some b -> 
                let backgroundExpresssion =backgroundExpression.CreateExpression feature b
                let backgroundField = PropertyHelper(feature).AddProperty("Background",backgroundExpresssion.Type)
                Some (backgroundExpresssion,backgroundField)

    member __.CreateExpression (providerName:string) (root:ProvidedTypeDefinition) (document:GherkinDocument) = 

            let (background,scenarios,tags) = getFeatureItemsFromDocument document 
            let featureName = sprintf "%s_Feature" providerName
            let featureType= ProvidedTypeDefinition(featureName,Some typeof<obj>,isErased=false, hideObjectMethods=true, isSealed=false)

            //Add Feature
            featureType |> root.AddMember

            let propertyHelper = PropertyHelper(featureType)

            // add name & description
            let nameField = propertyHelper.AddProperty("Name",typeof<string> )
            let descriptionField = propertyHelper.AddProperty("Description",typeof<string>)

            //get background if it exists
            let backgroundExpression = getBackgroundExpression featureType background
            let scenarioExpressions = scenarios |> List.map(scenarioExpressionBuilder.CreateExpression featureType)
            let scenarioParameters =  scenarioExpressions |> List.map(fun st -> ProvidedParameter(st.Name |> sanitize,st.Type))

            //untyped scenarios array - add the typed scenarios as scenariobase
            let scenariosType = scenarioBaseType.MakeArrayType()
            let scenariosField = propertyHelper.AddProperty("Scenarios",scenariosType)

            //create tags
            let tagContainerExpression = tagContainerExpressionBuilder.CreateExpression featureType tags

            //add the optional parameters
            let parameters = 
                let mandatory = [ProvidedParameter("name",typeof<string>);ProvidedParameter("description",typeof<string>)]
                match backgroundExpression,tagContainerExpression with
                | None,None -> mandatory
                | Some (backgroundType,_),None -> mandatory @ [ProvidedParameter("background",backgroundType.Type) ]
                | None,Some(tagType,_) -> mandatory @ [ProvidedParameter("tags",tagType)]
                | Some (backgroundType,_),Some(tagType,_) -> mandatory @ [ProvidedParameter("background",backgroundType.Type);ProvidedParameter("tags",tagType)]

            //create individual fields to hold the derived scenarios
            let scenarioFields = scenarioExpressions |> List.mapi(fun i sArg-> ProvidedField((sprintf "_scenario%i" i) |> sanitize, sArg.Type))

            //get the visited property of the scenario base
            let visitedProperty = scenarioBaseType.GetProperty("Visited")

            //properties named after the scenario names, accessing backing fields as typed scenarios
            let scenarioProperties = 
                List.map2(
                        fun  (scenarioExpression:ScenarioExpression) (scenarioField:ProvidedField) -> 
                        ProvidedProperty(
                            scenarioExpression.Name |> propertyNameSanitizer,
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
                                        scenarioFieldGet)

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
                        match backgroundExpression,tagContainerExpression with
                        | None,None -> args.GetSlice(Some 3,Some (args.Length-1))
                        | Some(_),None ->args.GetSlice(Some 4,Some (args.Length-1))
                        | Some(_),Some(_) ->args.GetSlice(Some 5,Some (args.Length-1))
                        | None,Some(_) -> args.GetSlice(Some 4,Some (args.Length-1))

                    //coerce the derived scenarios to their base class
                    let coercedParams = scenarios |> List.map (fun p -> Expr.Coerce(p,scenarioBaseType))

                    //then add them to the array 
                    let baseArray = Expr.NewArray(scenarioBaseType,coercedParams)

                    //set the array with the descriptors
                    let first = Expr.Sequential(Expr.FieldSet(this,scenariosField, baseArray),setDescriptors)
                    
                    //set each parameter to its non-derived backing field
                    let scenarioFieldSets = List.map2( fun scenarioField scenarioValue -> Expr.FieldSet(this,scenarioField,scenarioValue))  scenarioFields scenarios

                    //create a single expression with all the scenarios sets & the new array
                    let scenarioFieldSet = scenarioFieldSets |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) first
                    
                    // add any background and tags
                    let additionalSets =
                        match backgroundExpression,tagContainerExpression with
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
                Tags = match tagContainerExpression with | None -> None | Some (tagExpr,_) -> Some tagExpr
            }

    static member CreateNew (providerModel:GherkinProviderModel) (propertyNameSanitizer:string->string) =

        let dataExpressionBuilder = DataExpressionBuilder(
                                            providerModel.DataRowBaseType,
                                            providerModel.DataCellBaseType,
                                            propertyNameSanitizer)

        let stepExpressionBuilder = StepExpressionBuilder(
                                            providerModel.StepBaseType,
                                            providerModel.DocStringArgType,
                                            providerModel.ArgumentBaseType,
                                            dataExpressionBuilder)

        let tagContainerExpressionBuilder = TagContainerExpressionBuilder(
                                                providerModel.TagBaseType , 
                                                providerModel.TagContainerBaseType)

        let scenarioExpressionBuilder = ScenarioExpressionBuilder(
                                            providerModel.TagContainerBaseType,
                                            providerModel.TagBaseType,
                                            tagContainerExpressionBuilder,
                                            providerModel.DataRowBaseType,
                                            dataExpressionBuilder,
                                            providerModel.ScenarioBaseType,
                                            stepExpressionBuilder,
                                            providerModel.StepBaseType,
                                            propertyNameSanitizer)

        let emptyExamples = Expr.NewArray(providerModel.DataRowBaseType,[])
        let emptyTags = Expr.NewObject(providerModel.TagContainerBaseType.GetConstructors().[0],[Expr.NewArray(providerModel.TagBaseType.AsType(),[])])

        let backgroundExpressionBuilder = BackgroundExpressionBuilder(
                                            providerModel.ScenarioBaseType,
                                            emptyExamples,
                                            emptyTags,
                                            providerModel.StepBaseType,
                                            stepExpressionBuilder,
                                            propertyNameSanitizer)
        
        FeatureExpressionBuilder
            (backgroundExpressionBuilder,
            scenarioExpressionBuilder,
            providerModel.ScenarioBaseType,
            tagContainerExpressionBuilder,
            propertyNameSanitizer)