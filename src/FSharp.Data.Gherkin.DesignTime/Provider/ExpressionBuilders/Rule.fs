module ExpressionBuilders.Rule

open ProviderImplementation.ProvidedTypes
open Gherkin.Ast
open ObjectModel
open ExpressionBuilders.Scenario
open FSharp.Quotations
open Shared

let sanitize = Sanitizer().Sanitize

type RuleExpressionBuilder(ruleBaseType:System.Type,scenarioBaseType:System.Type,scenarioExpressionBuilder:ScenarioExpressionBuilder,propertyNameSanitizer) =
    member __.CreateExpression (parent:ProvidedTypeDefinition) (gherkinRule:Rule) =
       let name = sprintf "%sClass" gherkinRule.Name |> sanitize
       let ruleType = ProvidedTypeDefinition(name,Some ruleBaseType,isErased=false, hideObjectMethods=true, isSealed=false)
       ruleType |> parent.AddMember
       
        //typed example properties
       let examples =
            gherkinRule.Children
            |> Seq.cast<Scenario>
            |> Seq.toList
            
       let exampleExpressions =
            examples
            |> List.map(scenarioExpressionBuilder.CreateExpression ruleType)
            
       let exampleParameters =  exampleExpressions |> List.map(fun st -> ProvidedParameter(st.Name |> sanitize,st.Type))
       let exampleFields = exampleExpressions |> List.mapi(fun i st-> ProvidedField((sprintf "_example%i" i) |> sanitize, st.Type))
       
       //get the visited property of the scenario base
       let visitedProperty = scenarioBaseType.GetProperty("Visited")
       
       let exampleProperties =
            List.mapi2(
                fun i (example:Scenario) (exampleExpression:ScenarioExpression) ->
                    ProvidedProperty(
                        example.Name |> propertyNameSanitizer,
                        exampleExpression.Type,
                        getterCode=fun args->
                            //get the specific example field
                            let getExample = Expr.FieldGet(args.[0],exampleFields.[i])
                            let visitExample= Expr.PropertySet(getExample,visitedProperty,Expr.Value(true))
                            Expr.Sequential(visitExample,getExample)

                            )) examples exampleExpressions 

       exampleFields |> Seq.iter(ruleType.AddMember)
       exampleProperties |> Seq.iter(ruleType.AddMember)
       
       let ctr =
             ProvidedConstructor(
                [
                    ProvidedParameter("keyword",typeof<string>)
                    ProvidedParameter("name",typeof<string>)
                    ProvidedParameter("description",typeof<string>)
                ] @ exampleParameters,
                invokeCode = fun args ->
                      let examples = args.GetSlice(Some 4,Some (args.Length-1))
                      let exampleSets =
                          match 
                            examples |> List.mapi(fun i e -> Expr.FieldSet(args.[0],exampleFields.[i],e)) with
                           | [] -> [(<@@ () @@>)]
                           | sets -> sets
                      exampleSets.Tail |> List.fold(fun a c -> Expr.Sequential(a,c)) exampleSets.Head)
       
       let baseCtr = ruleBaseType.GetConstructors().[0]
       ctr.BaseConstructorCall <- fun (args:Expr list) ->
           let examples =
                args.GetSlice(Some 4,Some (args.Length-1))
                |> List.map(fun e -> Expr.Coerce(e,scenarioBaseType))
           let examplesArray = Expr.NewArray(scenarioBaseType,examples)
           baseCtr,[args.[0];args.[1];args.[2];args.[3];examplesArray]
           
       ctr |> ruleType.AddMember
        
       {
        Name = name
        Type= ruleType
        Examples=exampleExpressions
        }