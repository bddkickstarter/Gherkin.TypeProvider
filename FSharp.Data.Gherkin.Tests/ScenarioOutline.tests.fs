module FSharp.Data.Gherking.Tests.ScenarioOutline

open FSharp.Data.Gherkin
open Expecto

// Use the const __SOURCE_DIRECTORY__ here to help intellisense play nice with relative paths
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

let feature = TestFeature.``Feature name``