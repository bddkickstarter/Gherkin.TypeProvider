module ExpressionBuilders.Background

open ObjectModel
open ExpressionBuilders.Step
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open Shared
open BaseTypes.Step
open Gherkin.Ast

let sanitize = Sanitizer().Sanitize

type BackgroundExpressionBuilder 
            (scenarioBaseType:System.Type,
            stepBaseType:System.Type,
            stepsExpressionBuilder:StepExpressionBuilder,
            propertySanitizer:string->string) =

    member __.CreateExpression (feature:ProvidedTypeDefinition) (gherkinBackground:Background) =
    
        let backgroundType = ProvidedTypeDefinition("BackgroundClass",Some scenarioBaseType,isErased=false, hideObjectMethods=true, isSealed=false)
        backgroundType |> feature.AddMember

        //add step specific constructor params, properties & fields
        let backgroundStepList = gherkinBackground.Steps |> Seq.toList
        let stepExpressions =  backgroundStepList |> List.mapi(stepsExpressionBuilder.CreateExpression backgroundType)

        let parameters =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedParameter(StepBase.GetStepName(sanitize,i,step),stepExpression.Type)) backgroundStepList stepExpressions
        let stepFields =List.mapi2(fun i step (stepExpression:StepExpression) -> ProvidedField(StepBase.GetStepName(sanitize,i,step) ,stepExpression.Type))  backgroundStepList stepExpressions
        let visitedProperty = stepBaseType.GetProperty("Visited")

        let stepProperties = 
            List.mapi2(
                fun i step (stepExpression:StepExpression) -> 
                    ProvidedProperty(
                        StepBase.GetStepName(propertySanitizer,i,step),
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
        let baseCtr = scenarioBaseType.GetConstructors().[0]

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
                            stepFieldSets.Tail |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) stepFieldSets.Head)

        backgroundCtr.BaseConstructorCall <- 
            fun args -> 
                let steps = 
                    args.GetSlice(Some 3,Some(args.Length - 1))
                    |> List.map (fun s -> Expr.Coerce(s,stepBaseType))
                    
                baseCtr,[args.[0];args.[1];args.[2];Expr.NewArray(stepBaseType,steps)] // pass in name & descr to base class

        backgroundCtr |> backgroundType.AddMember
        
        {
            Type = backgroundType
            Steps = stepExpressions
        }



