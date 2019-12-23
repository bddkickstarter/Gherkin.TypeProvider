module ExpressionBuilders.RuleContainer

open ProviderImplementation.ProvidedTypes
open Gherkin.Ast
open ObjectModel
open Shared
open ExpressionBuilders.Rule
open FSharp.Quotations

let sanitize = Sanitizer().Sanitize

type RuleContainerExpressionBuilder(ruleContainerBase:System.Type,ruleBaseType:System.Type,ruleExpressionBuilder:RuleExpressionBuilder,propertyNameSanitizer) =
    
    member __.CreateExpression (parent:ProvidedTypeDefinition) (rules:Rule list) =
       
      let ruleContainerType = ProvidedTypeDefinition("RulesContainerClass",Some ruleContainerBase,isErased=false, hideObjectMethods=true, isSealed=false)
      ruleContainerType |> parent.AddMember
       
      //typed scenario properties
      let ruleExpressions = rules |> List.map(ruleExpressionBuilder.CreateExpression ruleContainerType)
      let ruleParameters =  ruleExpressions |> List.map(fun st -> ProvidedParameter(st.Name |> sanitize,st.Type))
      let ruleFields = ruleExpressions |> List.mapi(fun i st-> ProvidedField((sprintf "_scenario%i" i) |> sanitize, st.Type))
      
      //get the visited property of the scenario base
      let visitedProperty = ruleBaseType.GetProperty("Visited")
      
      //properties named after the scenario names, accessing backing fields as typed scenarios
      let ruleProperties =
            List.map2(fun(ruleExpression:RuleExpression) (ruleField:ProvidedField) -> 
                    ProvidedProperty(
                        ruleExpression.Name |> propertyNameSanitizer,
                        ruleExpression.Type,
                        isStatic = false,
                        getterCode = 
                            fun args -> 
                                //get the specific rule field
                                let ruleFieldGet = Expr.FieldGet(args.[0],ruleField)

                                Expr.Sequential(
                                    //visit rule
                                    Expr.PropertySet(ruleFieldGet,visitedProperty,Expr.Value(true)),
                                    //return rule
                                    ruleFieldGet)

                                )) ruleExpressions ruleFields

        // add fields & properties to feature
      ruleFields |> Seq.iter (ruleContainerType.AddMember)
      ruleProperties |> Seq.iter (ruleContainerType.AddMember)
      
      //constructor
      let ctr =
          ProvidedConstructor(
                      ruleParameters,
                      invokeCode = fun args ->
                        let this = args.Head
                        let ruleFieldSets  =
                                match args.Tail with
                                | [] -> [(<@@ () @@>)]
                                | rules -> List.map2( fun ruleField rule -> Expr.FieldSet(this,ruleField,rule))  ruleFields rules

                        //create a single expression with all the scenarios sets & the new array
                        ruleFieldSets.Tail |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) ruleFieldSets.Head)
          
      //override base constructor
      let baseCtr = ruleContainerBase.GetConstructors().[0]
      
      ctr.BaseConstructorCall <- fun args ->
                let this = args.Head
                let rules = args.Tail |> List.map(fun r -> Expr.Coerce(r,ruleBaseType))
                let rulesArray = Expr.NewArray(ruleBaseType,rules)
                baseCtr,[this;rulesArray]
                
      ctr |> ruleContainerType.AddMember
        
      {
        Type= ruleContainerType
        Rules= ruleExpressions
      }