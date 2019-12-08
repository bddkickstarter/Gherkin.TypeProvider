namespace FSharp.Data.Gherkin.Features

open FSharp.Data.Gherkin

// Use the const __SOURCE_DIRECTORY__ here to help intellisense play nice with relative paths
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>
type CSharpTestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature"),Sanitize=true>


    

