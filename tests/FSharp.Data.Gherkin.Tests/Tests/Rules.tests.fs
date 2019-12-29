module FSharp.Data.Gherkin.Tests.Rules

open Expecto
open FSharp.Data.Gherkin.Tests
open FSharp.Data.Gherkin.Tests.Helpers

let feature = RulesAndExamplesFeature.CreateFeature()

[<Tests>]
let rulesTests =
    testList 
        "Rules contain correct data"
        [
            testCase 
                "Rule 1 Name & Description correct" 
                <| fun _ ->
                    let rule = feature.Rules.``This is the first Rule``
                    Expect.equal rule.Name "This is the first Rule" (sprintf "Expected This is the first Rule but got %s" rule.Name)
                    Expect.equal (rule.Description.Trim()) "Rule 1 description" (sprintf "Expected Rule 1 description but got %s" rule.Description)

            testCase 
                "Rule 1 Correct number of examples" 
                <| fun _ ->
                    let rule = feature.Rules.``This is the first Rule``
                    Expect.equal rule.All.Length 1 (sprintf "Expected 1 but got %i" rule.All.Length)

            testCase 
                "Rule 2 Name & Description correct" 
                <| fun _ ->
                    let rule = feature.Rules.``This is the second Rule``
                    Expect.equal rule.Name "This is the second Rule" (sprintf "Expected This is the second Rule but got %s" rule.Name)
                    Expect.equal (rule.Description.Trim()) "Rule 2 description" (sprintf "Expected Rule 2 description but got %s" rule.Description)

            testCase 
                "Rule 2 Correct number of examples" 
                <| fun _ ->
                    let rule = feature.Rules.``This is the second Rule``
                    Expect.equal rule.All.Length 2 (sprintf "Expected 2 but got %i" rule.All.Length)
        ]

[<Tests>]
let rule1ExamplesTests =
    let rule = feature.Rules.``This is the first Rule``
    
    testList
        "Examples contain correct data"
        [
            let example1 = rule.``First example title``

            testCase
                "Example 1 is correct"
                <| fun _ -> validateExample example1 "First example title" "Example 1 description"

            testCase
                "Example 1 Given correct"
                <| fun _ ->

                    validateExampleStep
                        example1.``0 Given example 1 given``
                        0
                        "Given"
                        "example 1 given"

            testCase
                "Example 1 Given multiline argument correct"
                <| fun _ ->

                    let step = example1.``0 Given example 1 given``
                    let expectedArgument = "hello world"
                    Expect.equal 
                        (step.Argument.Content.Trim()) expectedArgument
                        (sprintf 
                            "Example 1 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Example 1 When correct"
                <| fun _ ->

                    validateExampleStep
                        example1.``1 When example 1 when``
                        1
                        "When"
                        "example 1 when"

            testCase
                "Example 1 When data table argument correct"
                <| fun _ ->

                    let step = example1.``1 When example 1 when``
                    Expect.equal step.Argument.Length 1 (sprintf "Scenario 1 When Data:Expected 1 rows but got %i" step.Argument.Length)

                    validateExampleStepData step.Argument.[0].hello "hello" "foo"
                    validateExampleStepData step.Argument.[0].world "world" "bar"

            testCase
                "Example 1 Then correct"
                <| fun _ ->

                    validateExampleStep
                        example1.``2 Then example 1 then``
                        2
                        "Then"
                        "example 1 then"
        ]

[<Tests>]
let rule2ExamplesTests =
    let rule = feature.Rules.``This is the second Rule``
    
    testList
        "Examples contain correct data"
        [
            let example2 = rule.``Second example title``

            testCase
                "Example 2 is correct"
                <| fun _ -> validateExample example2 "Second example title" "Example 2 description"

            testCase
                "Example 2 Given correct"
                <| fun _ ->

                    validateExampleStep
                        example2.``0 Given example 2 given``
                        0
                        "Given"
                        "example 2 given"

            testCase
                "Example 2 Given multiline argument correct"
                <| fun _ ->

                    let step = example2.``0 Given example 2 given``
                    let expectedArgument = "hello world again"
                    Expect.equal 
                        (step.Argument.Content.Trim()) expectedArgument
                        (sprintf 
                            "Example 2 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Example 2 When correct"
                <| fun _ ->

                    validateExampleStep
                        example2.``1 When example 2 when``
                        1
                        "When"
                        "example 2 when"

            testCase
                "Example 2 When data table argument correct"
                <| fun _ ->

                    let step = example2.``1 When example 2 when``
                    Expect.equal step.Argument.Length 1 (sprintf "Scenario 2 When Data:Expected 1 rows but got %i" step.Argument.Length)

                    validateExampleStepData step.Argument.[0].hello2 "hello2" "foo2"
                    validateExampleStepData step.Argument.[0].world2 "world2" "bar2"

            testCase
                "Example 2 Then correct"
                <| fun _ ->

                    validateExampleStep
                        example2.``2 Then example 2 then``
                        2
                        "Then"
                        "example 2 then"

            let example3 = rule.``Third example title``

            testCase
                "Example 3 is correct"
                <| fun _ -> validateExample example3 "Third example title" "Example 3 description"

            testCase
                "Example 3 Given correct"
                <| fun _ ->

                    validateExampleStep
                        example3.``0 Given example 3 given``
                        0
                        "Given"
                        "example 3 given"

            testCase
                "Example 3 Given multiline argument correct"
                <| fun _ ->

                    let step = example3.``0 Given example 3 given``
                    let expectedArgument = "hello world yet again"
                    Expect.equal 
                        (step.Argument.Content.Trim()) expectedArgument
                        (sprintf 
                            "Example 3 Given Argument:Expecting %s but got %s" 
                            expectedArgument 
                            step.Argument.Content)

            testCase
                "Example 3 When correct"
                <| fun _ ->

                    validateExampleStep
                        example3.``1 When example 3 when``
                        1
                        "When"
                        "example 3 when"

            testCase
                "Example 3 When data table argument correct"
                <| fun _ ->

                    let step = example3.``1 When example 3 when``
                    Expect.equal step.Argument.Length 1 (sprintf "Scenario 2 When Data:Expected 1 rows but got %i" step.Argument.Length)

                    validateExampleStepData step.Argument.[0].hello3 "hello3" "foo3"
                    validateExampleStepData step.Argument.[0].world3 "world3" "bar3"

            testCase
                "Example 3 Then correct"
                <| fun _ ->

                    validateExampleStep
                        example3.``2 Then example 3 then``
                        2
                        "Then"
                        "example 3 then"
        ]