module ExpressionBuilders.Scenario

open ExpressionBuilders
open ExpressionBuilders.BaseTypes
open ExpressionBuilders.Shared

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open ExpressionBuilders.Tags
open ExpressionBuilders.Data
open ExpressionBuilders.Step
open ExpressionBuilders.Global

open Gherkin.Ast

let createScenarioExpression (feature:ProvidedTypeDefinition) (gherkinScenario:Scenario) =

    let scenarioType = ProvidedTypeDefinition(gherkinScenario.Name |> SanitizeName,Some (ScenarioBaseType.AsType()),isErased=false)
    scenarioType |> feature.AddMember

    //create tags
    let tagExpression = createTagsExpression scenarioType (gherkinScenario.Tags |> Seq.toList |> List.map(fun t -> t.Name))

    //get the examples if any
    let exampleExpression = 
        match gherkinScenario.Examples |> Seq.toList with
        | [] -> None
        | examples -> 
            let columns = examples.[0].TableHeader.Cells |> Seq.map (fun c -> c.Value) |> Seq.toList
            let exampleType  = createDataExpression scenarioType columns
            let exampleField = ProvidedField("_examples",exampleType.MakeArrayType())
            let exampleProperty = ProvidedProperty("Examples",exampleType.MakeArrayType(),getterCode = fun args -> Expr.FieldGet(args.[0],exampleField))

            exampleField |> scenarioType.AddMember
            exampleProperty |> scenarioType.AddMember

            Some (exampleType,exampleField)

    //add the steps array backing field & property
    let stepsType = StepBaseType.MakeArrayType()
    let stepsField = ProvidedField("_steps",stepsType)
    let stepsProperty = ProvidedProperty("Steps",stepsType,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],stepsField))

    stepsField |> scenarioType.AddMember
    stepsProperty |> scenarioType.AddMember

    //add step specific constructor params, properties & fields
    let gherkinStepList = gherkinScenario.Steps |> Seq.toList
    let stepExpressions =  gherkinStepList |> List.mapi(createStepExpression scenarioType)

    let parameters = List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedParameter(getStepName i step,stepExpression.Type)) gherkinStepList stepExpressions 
    let stepFields = List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedField(getStepName i step,stepExpression.Type)) gherkinStepList stepExpressions 
    let visitedProperty = StepBaseType.GetProperty("Visited")

    let stepProperties = 
        List.mapi2(
            fun i step (stepExpression:StepExpression) -> 
                ProvidedProperty(
                    getStepName i step,
                    stepExpression.Type,
                    getterCode=fun args-> 

                        //get the specific step field
                        let stepField = Expr.FieldGet(args.[0],stepFields.[i])

                        Expr.Sequential(
                            //visit step
                            Expr.PropertySet(stepField,visitedProperty,Expr.Value(true)),
                            //return step
                            stepField
                            ))) gherkinStepList stepExpressions 

    stepFields |> Seq.iter(scenarioType.AddMember)
    stepProperties |> Seq.iter(scenarioType.AddMember)

    // override base constructor 
    let baseCtr = ScenarioBaseType.GetConstructors().[0]

    let constructorParams =
        match exampleExpression,tagExpression with
        | None,None -> (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: parameters)
        | Some (exampleType,_), None -> 
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("examples",exampleType.MakeArrayType()) :: parameters)
        | None, Some(tagType,_) ->
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("tags",tagType) :: parameters)
        | Some (exampleType,_),Some(tagType,_) ->
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("examples",exampleType.MakeArrayType()) :: ProvidedParameter("tags",tagType) :: parameters)

    let scenarioCtr = 
        ProvidedConstructor(
            constructorParams,
            invokeCode = 
                fun args -> 
                        let this = args.[0]

                        //get the steps from arguments based on whether there are examples & or tags
                        let steps = 
                            match exampleExpression,tagExpression with
                            | None,None -> args.GetSlice(Some 3,Some (args.Length-1))
                            | Some(_),None ->args.GetSlice(Some 4,Some (args.Length-1))
                            | Some(_),Some(_) ->args.GetSlice(Some 5,Some (args.Length-1))
                            | None,Some(_) -> args.GetSlice(Some 4,Some (args.Length-1))

                        // create the steps array
                        let coercedSteps = steps |> List.map(fun s -> Expr.Coerce(s,StepBaseType))
                        let stepsArray = Expr.NewArray(StepBaseType,coercedSteps)
                        let first = Expr.FieldSet(this,stepsField, stepsArray)

                        //set each parameter to its non-derived backing field
                        let stepFieldSets = List.map2( fun stepField stepValue -> Expr.FieldSet(this,stepField,stepValue))  stepFields steps

                        //create a single expression with all the step sets & the new array
                        let stepFieldSet = stepFieldSets |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) first

                        // add any background and tags
                        let additionalSets =
                            match exampleExpression,tagExpression with
                            | None,None -> []
                            | Some(_,backgroundField),None -> [Expr.FieldSet(this,backgroundField,args.[3])]
                            | Some(_,backgroundField),Some(_,tagField) -> [Expr.FieldSet(this,backgroundField,args.[3]);Expr.FieldSet(this,tagField,args.[4])]
                            | None,Some(_,tagField) ->  [Expr.FieldSet(this,tagField,args.[3])]

                        additionalSets |> Seq.fold(fun a c -> Expr.Sequential(a,c)) stepFieldSet

            )

    scenarioCtr.BaseConstructorCall <- fun args -> baseCtr,[args.[0];args.[1];args.[2]] // pass in name & descr to base class

    scenarioCtr |> scenarioType.AddMember
    
    {
        Name = gherkinScenario.Name
        Type = scenarioType
        Steps = stepExpressions
        Examples = match exampleExpression with | None -> None | Some(exampleExpr,_) -> Some exampleExpr
        Tags = match tagExpression with | None -> None | Some(tagExpr,_) -> Some tagExpr
    }