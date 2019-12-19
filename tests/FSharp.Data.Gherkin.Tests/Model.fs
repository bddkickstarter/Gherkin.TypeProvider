namespace FSharp.Data.Gherkin.Tests

open FSharp.Data.Gherkin

type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature"),Sanitize="partial">
type SimpleFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/simple.feature"),Sanitize="partial">
type OutlineFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/outline.feature"),Sanitize="partial">

module Helpers =

    open Expecto

    let validateFeature (feature:TestFeature.TestFeature_Feature) (name:string) (description:string) =
        Expect.equal feature.Name name (sprintf "Feature name:Expected %s but got %s" name feature.Name)
        Expect.equal feature.Description description (sprintf "Feature description:Expected %s but got %s" description feature.Description)

    let validateBackground (background:TestFeature.TestFeature_Feature.BackgroundClass) (name:string) (description:string) =
        Expect.equal background.Name name (sprintf "Background name:Expected %s but got %s" name background.Name)
        Expect.equal background.Description description (sprintf "Background description:Expected %s but got %s" description background.Description)

    let validateScenario (scenario:TestFeature.TestFeature_ScenarioBase) (name:string) (description:string) =
        Expect.equal scenario.Name name (sprintf "Scenario name:Expected %s but got %s" name scenario.Name)
        Expect.equal scenario.Description description (sprintf "Scenario description:Expected %s but got %s" description scenario.Description)

    let validateStep (step:TestFeature.TestFeature_StepBase) (expectedOrder:int) (expectedKeyword:string) (expectedText:string) =
        Expect.equal step.Order expectedOrder (sprintf "Step Order:Expected %i but got %i" expectedOrder step.Order)
        Expect.equal step.Keyword expectedKeyword (sprintf "Step Keyword:Expected %s but got %s" expectedKeyword step.Keyword)
        Expect.equal step.Text expectedText (sprintf "Step Text:Expected %s but got %s" expectedText step.Text)

    let validateData (row:TestFeature.TestFeature_DataCell) (header:string) (value:string) =
        Expect.equal row.Header header (sprintf "Background When Data Row 1 Header:Expected %s but got %s" header row.Header)
        Expect.equal row.Value value (sprintf "Background When Data Row 1 Value:Expected %s but got %s" value row.Value)