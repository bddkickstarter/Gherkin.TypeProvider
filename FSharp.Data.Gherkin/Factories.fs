namespace FSharp.Data.Gherkin.Factories

open Gherkin
open FSharp.Data.Gherkin
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

// module Background =

//     let createbackgroundInstance (gherkinBackground:Ast.Background) =
//         let backgroundCtr = typeof<Background>.GetConstructors().[0]
//         Expr.NewObject(backgroundCtr,[Expr.Value(gherkinBackground.Name);Expr.Value(gherkinBackground.Description)])


//     let createBackgroundType (gherkinBackground:Ast.Background) =

//         let background = ProvidedTypeDefinition(gherkinBackground.Name, Some typeof<Background>, isErased=false, hideObjectMethods=true, nonNullable=true)
        
//         gherkinBackground.Steps
//         |> Seq.iteri(
//             fun i s ->
//                 let stepName = sprintf "%i. %s%s" i s.Keyword s.Text
//                 let step = createStep s i s.Text |> addArgument s.Argument
//                 step |> background.AddMember 

//                 ProvidedProperty(stepName,step.AsType(),isStatic = false, getterCode=fun _ -> <@@ obj() @@>) |> background.AddMember)
        
//         background