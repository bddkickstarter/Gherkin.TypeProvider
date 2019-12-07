module ExpressionBuilders.Background

open ExpressionBuilders
open ExpressionBuilders.Shared
open ExpressionBuilders.Step
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

open Gherkin.Ast

let createBackgroundExpression  (feature:ProvidedTypeDefinition)  (gherkinBackground:Background) =
    let backgroundType = ProvidedTypeDefinition("Background",Some (ScenarioBaseType.Value.AsType()),isErased=false)
    backgroundType |> feature.AddMember


    //add the steps array backing field & property
    let stepsType = StepBaseType.Value.MakeArrayType()
    let stepsField = ProvidedField("_steps",stepsType)
    let stepsProperty = ProvidedProperty("Steps",stepsType,isStatic=false,getterCode=fun args -> Expr.FieldGet(args.[0],stepsField))

    stepsField |> backgroundType.AddMember
    stepsProperty |> backgroundType.AddMember

    //add step specific constructor params, properties & fields
    let backgroundStepList = gherkinBackground.Steps |> Seq.toList
    let stepExpressions =  backgroundStepList |> List.mapi(createStepExpression backgroundType)

    let parameters =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedParameter(getStepName i step,stepExpression.Type)) backgroundStepList stepExpressions
    let stepFields =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedField(getStepName i step,stepExpression.Type))  backgroundStepList stepExpressions
    let visitedProperty = StepBaseType.Value.GetProperty("Visited")

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
                            ))) backgroundStepList stepExpressions

    stepFields |> Seq.iter(backgroundType.AddMember)
    stepProperties |> Seq.iter(backgroundType.AddMember)

    // override base constructor 
    let baseCtr = ScenarioBaseType.Value.GetConstructors().[0]

    let backgroundCtr = 
        ProvidedConstructor(
            ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: parameters,
            invokeCode = 
                fun args -> 
                        let this = args.[0]

                        //get the steps from arguments (after name & desc)
                        let steps = args.GetSlice(Some 3,Some (args.Length-1))

                        // create the steps array
                        let coercedSteps = steps |> List.map(fun s -> Expr.Coerce(s,StepBaseType.Value))
                        let stepsArray = Expr.NewArray(StepBaseType.Value,coercedSteps)
                        let first = Expr.FieldSet(this,stepsField, stepsArray)

                        //set each parameter to its non-derived backing field
                        let stepFieldSets = List.map2( fun stepField stepValue -> Expr.FieldSet(this,stepField,stepValue))  stepFields steps

                        //create a single expression with all the step sets & the new array
                        stepFieldSets |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) first

            )

    backgroundCtr.BaseConstructorCall <- fun args -> baseCtr,[args.[0];args.[1];args.[2]] // pass in name & descr to base class

    backgroundCtr |> backgroundType.AddMember
    
    {
        Type = backgroundType
        Steps = stepExpressions
    }