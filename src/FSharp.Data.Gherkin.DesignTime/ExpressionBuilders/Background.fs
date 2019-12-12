module ExpressionBuilders.Background

open ExpressionBuilders
open ExpressionBuilders.Shared
open ExpressionBuilders.Step
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

open Gherkin.Ast

let createBackgroundExpression (context:GeneratedTypeContext)  (feature:ProvidedTypeDefinition)  (gherkinBackground:Background) =
    let backgroundType = ProvidedTypeDefinition("BackgroundClass",Some (context.ScenarioBaseType.AsType()),isErased=false, hideObjectMethods=true, isSealed=false)
    backgroundType |> feature.AddMember

    // //add the steps array backing field & property
    // let stepsType = context.StepBaseType.MakeArrayType()
    // let stepsField =addProperty backgroundType "Steps" stepsType
    
    //add step specific constructor params, properties & fields
    let backgroundStepList = gherkinBackground.Steps |> Seq.toList
    let stepExpressions =  backgroundStepList |> List.mapi(createStepExpression context backgroundType)

    let parameters =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedParameter(getStepName context.SanitizeName i step,stepExpression.Type)) backgroundStepList stepExpressions
    let stepFields =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedField(getStepName context.SanitizeName i step,stepExpression.Type))  backgroundStepList stepExpressions
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
                            ))) backgroundStepList stepExpressions

    stepFields |> Seq.iter(backgroundType.AddMember)
    stepProperties |> Seq.iter(backgroundType.AddMember)

    // override base constructor 
    let baseCtr = context.ScenarioBaseType.GetConstructors().[0]

    let backgroundCtr = 
        ProvidedConstructor(
            ProvidedParameter("name",typeof<string>) :: ProvidedParameter("description",typeof<string>) :: parameters,
            invokeCode = 
                fun args -> 
                        let this = args.[0]

                        //get the steps from arguments (after name & desc)
                        let steps = args.GetSlice(Some 3,Some (args.Length-1))

                        //set each parameter to its non-derived backing field
                        let stepFieldSets = List.map2( fun stepField stepValue -> Expr.FieldSet(this,stepField,stepValue))  stepFields steps

                        //create a single expression with all the step sets & the new array
                        stepFieldSets.Tail |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) stepFieldSets.Head

            )

    backgroundCtr.BaseConstructorCall <- 
        fun args -> 
            let steps = 
                args.GetSlice(Some 3,Some(args.Length - 1))
                |> List.map (fun s -> Expr.Coerce(s,context.StepBaseType.AsType()))
                
            baseCtr,[args.[0];args.[1];args.[2];Expr.NewArray(context.StepBaseType,steps)] // pass in name & descr to base class

    backgroundCtr |> backgroundType.AddMember
    
    {
        Type = backgroundType
        Steps = stepExpressions
    }