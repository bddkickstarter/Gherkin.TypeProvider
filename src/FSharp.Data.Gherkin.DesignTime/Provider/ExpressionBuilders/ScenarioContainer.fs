module ExpressionBuilders.ScenarioContainer

open BaseTypes.Scenario
open BaseTypes.ScenarioContainer
open ProviderImplementation.ProvidedTypes
open Gherkin.Ast
open ObjectModel
open ExpressionBuilders.Scenario
open FSharp.Quotations
open Shared

let sanitize = Sanitizer().Sanitize

type ScenarioContainerExpressionBuilder(scenarioContainerBase:System.Type,scenarioBaseType:System.Type,scenarioExpressionBuilder:ScenarioExpressionBuilder,propertyNameSanitizer) =
        member __.CreateExpression (parent:ProvidedTypeDefinition) (scenarios:Scenario list) =
                
                let scenarioContainerType = ProvidedTypeDefinition("ScenarioContainer",Some (scenarioContainerBase),isErased =false,hideObjectMethods=true, isSealed=false)
                scenarioContainerType |> parent.AddMember
                               
                //typed scenario properties
                let scenarioExpressions = scenarios |> List.map(scenarioExpressionBuilder.CreateExpression scenarioContainerType)
                let scenarioParameters =  scenarioExpressions |> List.map(fun st -> ProvidedParameter(st.Name |> sanitize,st.Type))
                let scenarioFields = scenarioExpressions |> List.mapi(fun i st-> ProvidedField((sprintf "_scenario%i" i) |> sanitize, st.Type))

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
                scenarioFields |> Seq.iter (scenarioContainerType.AddMember)
                scenarioProperties |> Seq.iter (scenarioContainerType.AddMember)

                let ctr =
                    ProvidedConstructor(
                            scenarioParameters,
                            invokeCode = fun args ->
                        //get the scenarios from arguments based on whether there are tags and/or background
                        let this = args.Head
                        let scenarios = args.Tail

                        //set each parameter to its non-derived backing field
                        let scenarioFieldSets = List.map2( fun scenarioField scenarioValue -> Expr.FieldSet(this,scenarioField,scenarioValue))  scenarioFields scenarios

                        //create a single expression with all the scenarios sets & the new array
                        scenarioFieldSets.Tail |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) scenarioFieldSets.Head
                        )
                
                //override base constructor
                let baseCtr = scenarioContainerBase.GetConstructors().[0]
                
                ctr.BaseConstructorCall <- fun args ->
                        let this = args.Head
                        let scenarios = args.Tail
                
                        //coerce the derived scenarios to their base class
                        let coercedParams = scenarios |> List.map (fun s -> Expr.Coerce(s,scenarioBaseType))

                        //then add them to the array 
                        let baseArray = Expr.NewArray(scenarioBaseType,coercedParams)
                        
                        baseCtr,[this;baseArray]
                
                ctr |> scenarioContainerType.AddMember
                    
                {
                        Type = scenarioContainerType
                        Scenarios = scenarioExpressions
                }