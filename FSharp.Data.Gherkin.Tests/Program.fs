open Expecto

//open FSharp.Data.Gherkin.New
open System.Collections.Generic
open System.Collections
open System.Reflection
open Gherkin

open FSharp.Data.Gherkin
open ExpressionBuilders.Feature
open InstanceBuilders.Feature

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type TestFeature = FSharp.Data.Gherkin.GherkinProvider<"C:\\src\\bddkickstarter\\Gherkin.TypeProvider\\FSharp.Data.Gherkin.Tests\\test.feature">

 [<EntryPoint>]
 let main argv =

    let feature = TestFeature.Feature
   //  let featureDesc = feature.Description
   //  let scenario1 = feature.basic_scenario_no_examples_or_arguments.Description
   //  let scenario1Given = feature.basic_scenario_no_examples_or_arguments._0_a_simple_given_step.Text

   //  let background = feature.Background._0_background_given.Text

   //  let docStringArg = feature.another_basic_scenario_no_examples_or_arguments._0_another_simple_given_step.Argument.Content

   //  let dataArg = feature.basic_scenario_no_examples_or_arguments._2_a_simple_then_step.Argument.[0].col_1

   //  let scenarioExample = feature.a_scenario_outline.Examples.[0].exmp_1
    

   //  // let ns = "FSharp.Data.Gherkin.TotallyNew"

   //  // let providedAssembly = ProvidedAssembly() 
   //  // let root = ProvidedTypeDefinition(providedAssembly,ns,"Root",Some typeof<obj>,isErased=false)

   //  // let gherkinDocument = Parser().Parse("C:\\src\\bddkickstarter\\Gherkin.TypeProvider\\FSharp.Data.Gherkin.Tests\\reallybasic.feature")

   //  // let featureExpression = createFeatureExpression gherkinDocument
   //  // featureExpression.Type |> root.AddMember

   //  // let feature = buildFeature featureExpression gherkinDocument
   
    
   // //  ProvidedProperty("Feature",featureExpression.Type,isStatic=true,getterCode=fun _ -> feature)
   // //  |> root.AddMember

   //  printfn "Fetaure:%A " featureDesc 
   //  printfn "Scenario 1:%A " scenario1
   //  printfn "Scenario1 Given:%A " scenario1Given
   //  printfn "Background:%A " background
   //  printfn "Doc string arg:%A " docStringArg
   //  printfn "Data arg:%A " dataArg
   //  printfn "Example:%A " scenarioExample


    printfn "got %A" feature    

    runTestsInAssembly defaultConfig argv