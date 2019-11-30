module FSharp.Data.Gherking.Tests.UnTyped

open FSharp.Data.Gherkin
open FSharp.Data.Gherkin.Tests.Model
open FSharp.Data.Gherkin.Tests.Model.Helpers

open Expecto

let untypedFeature = TestFeature.``Feature name``.Feature
let untypedBackground = untypedFeature.Background
let untypedScenario1 = untypedFeature.Scenarios |> Seq.head
let untypedScenario2 = untypedFeature.Scenarios |> Seq.tail |> Seq.head

let validateStep (step:Step) (order:int) (keyword:string) (text:string) =
    Expect.equal order step.Order (sprintf "Step Order:Expected %i but got %i" order step.Order)
    Expect.equal keyword step.Keyword (sprintf "Step Order:Expected %s but got %s" keyword step.Keyword)
    Expect.equal text step.Text (sprintf "Step Order:Expected %s but got %s" text step.Text)


[<Tests>]
let untypedFeatureTest =

    testCase
        "Untyped feature correct"
        <| fun _ -> 
            Expect.equal untypedFeature.Name "Feature name" (sprintf "Feature name:Expected %s but got %s" "Feature name" untypedFeature.Name)
            Expect.equal untypedFeature.Description  "Multi-line\r\nFeature Description" (sprintf "Feature Description:Expected %s but got %s"  "Multi-line\r\nFeature Description" untypedFeature.Description)
            

[<Tests>]
let untypedBackgroundTest = 
    let untypedBackground = untypedFeature.Background

    testCase
        "Untyped background correct"
        <| fun _ ->
            Expect.equal untypedBackground.Name "Background name" (sprintf "Background name:Expected %s but got %s" "Backround name" untypedBackground.Name)
            Expect.equal untypedBackground.Description "Multi-line\r\nBackground Description" (sprintf "Background description:Expected %s but got %s" "Multi-line\r\nBackground Description" untypedBackground.Description)

            

[<Tests>]
let untypedBackgroundStepsTest = 
    let untypedBackgroundSteps = untypedFeature.Background.Steps |> Seq.toList

    testCase
        "Untyped background steps correct"
        <| fun _ ->
            Expect.equal untypedBackgroundSteps.Length 3 (sprintf "Background steps length:Expected %i but got %i" 3 untypedBackgroundSteps.Length)
            validateStep untypedBackgroundSteps.[0] 0 "Given" "background given step"
            validateStep untypedBackgroundSteps.[1] 1 "When" "background when step"
            validateStep untypedBackgroundSteps.[2] 2 "Then" "background then step"

[<Tests>]
let untypedScenariosTest = 
    let untypedScenarios = untypedFeature.Scenarios |> Seq.toList

    testCase
        "Untyped scenarios correct"
        <| fun _ ->
            Expect.equal untypedScenarios.Length 2 (sprintf "Scenarios length:Expected %i but got %i" 2 untypedScenarios.Length)

[<Tests>]
let untypedScenario1Test = 

    testCase
        "Untyped scenario 1 correct"
        <| fun _ ->
            Expect.equal untypedScenario1.Name "Scenario 1 name" (sprintf "Scenario 1 name:Expected %s but got %s" "Scenario 1 name" untypedScenario1.Name)
            Expect.equal untypedScenario1.Description "Multi-line\r\nScenario 1 Description" (sprintf "Scenario 1 description:Expected %s but got %s" "Multi-line\r\nScenario 1 Description" untypedScenario1.Description)

[<Tests>]
let untypedScenario1StepsTest = 
    let untypedScenario1Steps = untypedScenario1.Steps |> Seq.toList

    testCase
        "Untyped scenario 1 steps correct"
        <| fun _ ->
            validateStep untypedScenario1Steps.[0] 0 "Given" "scenario 1 given step"
            validateStep untypedScenario1Steps.[1] 1 "When" "scenario 1 when step"
            validateStep untypedScenario1Steps.[2] 2 "Then" "scenario 1 then step"


[<Tests>]
let untypedScenario2Test = 

    testCase
        "Untyped scenario 2 correct"
        <| fun _ ->
            Expect.equal untypedScenario2.Name "Scenario 2 name" (sprintf "Scenario 2 name:Expected %s but got %s" "Scenario 2 name" untypedScenario2.Name)
            Expect.equal untypedScenario2.Description "Multi-line\r\nScenario 2 Description" (sprintf "Scenario 2 description:Expected %s but got %s" "Multi-line\r\nScenario 2 Description" untypedScenario2.Description)


[<Tests>]
let untypedScenario2StepsTest = 
    let untypedScenario2Steps = untypedScenario2.Steps |> Seq.toList

    testCase
        "Untyped scenario 2 steps correct"
        <| fun _ ->
            validateStep untypedScenario2Steps.[0] 0 "Given" "scenario 2 given step"
            validateStep untypedScenario2Steps.[1] 1 "When" "scenario 2 when step"
            validateStep untypedScenario2Steps.[2] 2 "Then" "scenario 2 then step"



    

            

        