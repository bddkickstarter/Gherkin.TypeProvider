namespace FSharp.Data.Gherkin.Tests.Model

open FSharp.Data.Gherkin

// Use the const __SOURCE_DIRECTORY__ here to help intellisense play nice with relative paths
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

module Helpers =
    
    open Expecto

    let validateBackground (background:Background) (name:string) (description:string) =
        Expect.equal background.Name name (sprintf "Background name:Expected %s but got %s" name background.Name)
        Expect.equal background.Description description (sprintf "Background description:Expected %s but got %s" description background.Description)

    let validateScenario (scenario:Scenario) (name:string) (description:string) =
        Expect.equal scenario.Name name (sprintf "Scenario name:Expected %s but got %s" name scenario.Name)
        Expect.equal scenario.Description description (sprintf "Scenario description:Expected %s but got %s" description scenario.Description)

    let validateStep (step:Step) (expectedOrder:int) (expectedKeyword:string) (expectedText:string) =
        Expect.equal step.Order expectedOrder (sprintf "Step Order:Expected %i but got %i" expectedOrder step.Order)
        Expect.equal step.Keyword expectedKeyword (sprintf "Step Keyword:Expected %s but got %s" expectedKeyword step.Keyword)
        Expect.equal step.Text expectedText (sprintf "Step Text:Expected %s but got %s" expectedText step.Text)

    let validateDatarow (row:DataCell) (header:string) (value:string) =
        Expect.equal row.Header header (sprintf "Background When Data Row 1 Header:Expected %s but got %s" header row.Header)
        Expect.equal row.Value value (sprintf "Background When Data Row 1 Value:Expected %s but got %s" value row.Value)