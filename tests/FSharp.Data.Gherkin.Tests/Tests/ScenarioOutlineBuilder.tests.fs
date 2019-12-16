module FSharp.Data.Gherkin.Tests.ScenarioOutlineBuilder

open FSharp.Data.Gherkin.Builders
open Expecto
open FSharp.Data.Gherkin.Tests

let (>>=) (name,tests) testList  = testList name tests

[<Tests>]
let useBuilder =
    let scenarioTemplate = TestFeature.CreateFeature().``Scenario outline name``

    ScenarioOutline(scenarioTemplate){
        return!
            fun scenario ->
                test scenario.Name {

                    Expect.isTrue
                        (scenario.``0 Given scenario outline given step _Example Column 1_``.Text.Length > 0)
                        "Given not copied correctly"

                    Expect.isTrue
                        (scenario.``1 When scenario outline when step _Example Column 2_``.Text.Length > 0)
                        "When not copied correctly"

                    Expect.isTrue
                        (scenario.``2 Then scenario outline then step _Example Column 3_``.Text.Length > 0)
                        "Then not copied correctly"

                    Expect.isTrue 
                        (scenario.``0 Given scenario outline given step _Example Column 1_``.Argument.Content.Length >0)
                        "Doc string argument not copied correctly"

                    Expect.isTrue 
                        (scenario.``1 When scenario outline when step _Example Column 2_``.Argument.Length >0)
                        "Data table argument not copied correctly"

                    Expect.equal 
                        scenario.Steps.Length scenarioTemplate.Steps.Length
                        "Steps not copied correctly"
                }
    } >>= testList
