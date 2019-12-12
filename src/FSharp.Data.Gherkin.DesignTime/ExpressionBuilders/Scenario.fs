module ExpressionBuilders.Scenario

open ExpressionBuilders
open ExpressionBuilders.Shared
open ExpressionBuilders.Tags
open ExpressionBuilders.Data
open ExpressionBuilders.Step

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

open Gherkin.Ast

let createScenarioExpression (context:GeneratedTypeContext) (feature:ProvidedTypeDefinition) (gherkinScenario:Scenario) =

    let scenarioType = ProvidedTypeDefinition((sprintf "%sClass" gherkinScenario.Name) |> sanitize,Some (context.ScenarioBaseType.AsType()),isErased=false, hideObjectMethods=true, isSealed=false)
    scenarioType |> feature.AddMember

    //create tags
    let tagExpression = createTagsExpression context scenarioType (gherkinScenario.Tags |> Seq.toList |> List.map(fun t -> t.Name))

    //get the examples if any
    let exampleExpression = 
        match gherkinScenario.Examples |> Seq.toList with
        | [] -> None
        | examples -> 
            let columns = examples.[0].TableHeader.Cells |> Seq.map (fun c -> c.Value) |> Seq.toList
            let exampleType  = createDataExpression context scenarioType columns
            let exampleField = addProperty scenarioType "Examples" (exampleType.MakeArrayType())
            Some (exampleType,exampleField)

    //add the steps array backing field & property
    let stepsType = context.StepBaseType.MakeArrayType()
    let stepsField = addProperty scenarioType "Steps" stepsType

    //add step specific constructor params, properties & fields
    let gherkinStepList = gherkinScenario.Steps |> Seq.toList
    let stepExpressions =  gherkinStepList |> List.mapi(createStepExpression context scenarioType)

    let parameters = List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedParameter((getStepName context.SanitizeName i step) |> sanitize ,stepExpression.Type)) gherkinStepList stepExpressions 
    let stepFields = List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedField((getStepName context.SanitizeName i step) |> sanitize  ,stepExpression.Type)) gherkinStepList stepExpressions 
    let visitedProperty = context.StepBaseType.GetProperty("Visited")

    let stepProperties = 
        List.mapi2(
            fun i step (stepExpression:StepExpression) -> 
                ProvidedProperty(
                    getStepName context.SanitizeName i step,
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
    let baseCtr = context.ScenarioBaseType.GetConstructors().[0]

    let constructorParams =
        match exampleExpression,tagExpression with
        | None,None -> (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: parameters)
        | Some (exampleType,_), None -> 
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("examples",exampleType.MakeArrayType()) :: parameters)
        | None, Some(tagType,_) ->
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("tags",tagType) :: parameters)
        | Some (exampleType,_),Some(tagType,_) ->
            (ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: ProvidedParameter("examples",exampleType.MakeArrayType()) :: ProvidedParameter("tags",tagType) :: parameters)

    let getStepsFromArgs (args:Expr list) examples tags =
            //get the steps from arguments based on whether there are examples & or tags
            match examples,tags with
            | None,None -> args.GetSlice(Some 3,Some (args.Length-1))
            | Some(_),None ->args.GetSlice(Some 4,Some (args.Length-1))
            | Some(_),Some(_) ->args.GetSlice(Some 5,Some (args.Length-1))
            | None,Some(_) -> args.GetSlice(Some 4,Some (args.Length-1))

    let scenarioCtr = 
        ProvidedConstructor(
            constructorParams,
            invokeCode = 
                fun args -> 
                        let this = args.[0]

                        let steps = getStepsFromArgs args exampleExpression tagExpression

                        // create the steps array
                        let coercedSteps = steps |> List.map(fun s -> Expr.Coerce(s,context.StepBaseType))
                        let stepsArray = Expr.NewArray(context.StepBaseType,coercedSteps)
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

    scenarioCtr.BaseConstructorCall <- 
        fun args -> 
            let steps = 
                getStepsFromArgs args exampleExpression tagExpression
                |> List.map(fun s -> Expr.Coerce(s,context.StepBaseType))
                
            let stepsArray = Expr.NewArray(context.StepBaseType.AsType(),steps)
           
            baseCtr,[args.[0];args.[1];args.[2];stepsArray] // pass in name,descr & all the steps as an array to base class

    scenarioCtr |> scenarioType.AddMember
    
    {
        Name = gherkinScenario.Name
        Type = scenarioType
        Steps = stepExpressions
        Examples = match exampleExpression with | None -> None | Some(exampleExpr,_) -> Some exampleExpr
        Tags = match tagExpression with | None -> None | Some(tagExpr,_) -> Some tagExpr
    }