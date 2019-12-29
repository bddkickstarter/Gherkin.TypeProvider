# Gherkin.TypeProvider

### Background

The [Gherkin](https://cucumber.io/docs/gherkin/reference/) language provides a strict schema that has an existing [parser](https://www.nuget.org/packages/gherkin/) used by the major Gherkin parsing automation tools such as Specflow or the Cucumber family.

The parser is used by the Gherkin typeprovider to parse the supplied feature file to expose the Gherkin abstract syntax tree:

* A Feature has an array of children, that can be a Background, Scenario or Scenario Outline.
* A feature child has an array of steps which have various properties such as keyword and text
* A step can optionally have an argument which can be either a document string or a data table
* A scenario can have an optional array of examples
* Features & scenarios can have an optional array of tags

The Gherkin Type Provider simply adds to this structure by creating named properties for important elements in the Feature.  Those elements are:

* Scenario & Scenario Outline names
* Step keyword, order & text
* Tag names
* Data in either DocString/DataTable step arguments, or Example tables

Referencing the properties generated from the text of the feature means that if the feature file changes then the property names will change, and code referencing the old property names will fail to compile.

## Usage

* ### Add the package [__Gherkin.TypeProvider__](https://www.nuget.org/packages/Gherkin.TypeProvider)


* ### Reference the FSharp.Data.Gherkin namespace
    
    ```fsharp
    open FSharp.Data.Gherkin
    ```

* ### Create a type based on a feature file
    
    Using the const method & Source Directory constant to ensure the correct directory is searched for the feature file
  
    ```fsharp
    type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>
    ```

* ### Create an instance of the feature

    ```fsharp
    let myTestFeature = TestFeature.CreateFeature()
    ```

# Scenarios & Steps

If we had the following feature

```gherkin
Feature: a feature

Scenario: this is a scenario
    Given this is a given step
```

Then the Gherkin Typeprovider would expose that as:

```fsharp
    myTestFeature
        .Scenarios
        .``this is a scenario``
        .``0 Given this is a given step``
```

The object returned by the named step has the information from the feature file so,

```fsharp
    let step = 
        myTestFeature
            .Scenarios
            .``this is a scenario``
            .``0 Given this is a given step``

    printfn 
        "Order: %i, Keyword: %s, Text: %s" 
        step.Order 
        step.Keyword 
        step.Text
```

would print out:

```console
Order: 0, Keyword: Given, Text: this is a given step
```
# Tags
Tags can be used with Features, Scenarios & Scenario Outlines and are available via the typed *Tags* property

With the feature

```gherkin
@featuretag
Feature: a feature

@scenariotag
Scenario: this is a scenario
    Given this is a given step
```

```fsharp
myTestFeature.Tags.featuretag.Name
```
would return

```
@featuretag
```

And

```fsharp
myTestFeature.Scenarios.``this is a scenario``.Tags.scenariotag.Name
```
would return

```console
@scenariotag
```

# Step Arguments
Steps can have document string arguments:

```gherkin
Feature: a feature

Scenario: this is a scenario

    Given this is a given step
    When a when step with a string argument
    """
        this is a multi
        line string argument
    """
```

which would be represented as:

```fsharp
myTestFeature
    .Scenarios
    .``this is a scenario``
    .``1 a when step with a string argument``
    .Argument
    .Content
```

The **Argument** here is of type _DocString_ so returns an objet that has the _Content_ and _ContentType_ properties containing the values from the feature file.

This would output:

```console
this is a multi
line string argument
```

But an argument can also be a data table, what then? 

As the type system is generated, an argument on a specific senario can be any type we please.  
If a step has a string argument it is accessed as above, but if it has a data table argument, it is used slightly different.

In this feature we're using a data table argument with the When step:

```gherkin
Feature: a feature

Scenario: this is a scenario

    Given this is a given step
    When a when step with a data table argument
    |Column 1| Column 2|
    | Data 1 | Data 2  |
    | Data 3 | Data 4  |
```

Here the **Argument** is no longer a _DocString_,but instead an array of data objects that have individual _DataCells_ as named properties.

```fsharp
myTestFeature
    .Scenarios
    .``this is a scenario``
    .``1 a when step with a data table argument``
    .Argument
    .[1]
    .``Column 2``
    .Value
```

Would output:

```console
Data 4
```

So by accessing the second row of the argument we are selecting the second row of the data table.  

Then by using the named column __``Column 2``__, the _DataCell_ for the 2nd row & 2nd column is returned.

A _DataCell_ has 2 properties; Header & Value.  The header contains the name of the cell's column, while the value is the contents of the cell


# Scenario Outlines & Examples

Examples are just a data table and are accessed in a similar way to the data table argument.

With the following feature

```gherkin
Feature: Outlines

Scenario Outline: some group of examples
Given some setup
When uses <uses the data>
Then result will be <checks the result>

Examples:
|uses the data|checks the result|
|    data 1   |     true!       |
|    data 2   |     false!      |
|    data 3   |     maybe?      |
```

The data from the second column of the first row would be accessed using:

```fsharp
myTestFeature
    .Scenarios
    .``some group of examples``
    .Examples
    .[1]
    .``checks the result``
    .Value
```

And would evaluate to:

```console
false!
```

# Sanitizing

There are 3 options for sanitizing the property names:
  
  - **none**: the default.  Only illegal F# characters will be replaced with underscores
  - **partial**: all non alpha numeric characters except spaces are replaced with underscores 
  - **full**: all non alpha numeric characters are replaced with underscores 

and are specified using the Sanitize option when creating the type:

```fsharp
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature"), Sanitize="partial">
```

# Consuming from C#

To consume the type system in a C# project create an F# project to host the types.

As C# won't recognised properties with illegal characters in them (such as spaces)  santize the names by using the Santize full option:

```fsharp
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature"), Sanitize="full">
```

Which will replace illegal characters with underscores.  The previous example now looks like this:

```csharp
myTestFeature
    .Scenarios
    .some_group_of_examples
    .Examples
    .[0]
    .checks_the_result
    .Value
```

Reference the F# project containing the sanitized type system and take advantage of a typed gherkin file in C#.

There is an example of an F# wrapper [here](https://github.com/bddkickstarter/Gherkin.TypeProvider/tree/master/tests/FSharp.Data.Gherkin.Tests.CSharp.Shim/Library.fs) which is then consumed in a C# console app [here](https://github.com/bddkickstarter/Gherkin.TypeProvider/blob/master/tests/FSharp.Data.Gherkin.Tests.CSharp/Program.cs)


# The __Visited__ property

So far the code & the feature file have been kept in sync by referencing the named properties (scenarios, steps etc) directly.  If the scenario name changes, the property based on the name will change and the build will fail.

But what about properties that haven't been referenced (either because they haven't been used in the test code, or a completely new scenario, step or column name is added to the feature file)?

To keep the code and feature file complete in sync when a named property is accessed it sets a _Visited_ property to true on the underlying field.

The underlying field can be accessed via an array on the parent (a Scenarios array for a feature, Steps array for a Scenario etc etc) as each property derives from a base class/

For example,when the scenario from the first feature is accessed like:

```fsharp
let scenario = 
    myTestFeature
        .Scenarios
        .``this is a scenario``
```

The type of the scenario will be 

```fsharp
``this_is_a_scenarioClass`` 
```

However the feature also contains an array of all its Scenarios as their base classes (_TestFeature_ScenarioBase_), so if the only scenario in this feature was accessed like:

```fsharp
let scenario = 
    myTestFeature
        .Scenarios.All.[0]
```

The type of the scenario will be 

```fsharp
``TestFeature_ScenarioBase`` 
```

The base class has the _Visited_ property which is set to true __only__ when the named property is used, so the following test will pass:

```fsharp
let before = myTestFeature.Scenarios.All.[0].Visited

Expect.isFalse before "Should be false"

myTestFeature.Scenarios.``this is a scenario`` |> ignore

let after = myTestFeature.Scenarios.All.[0].Visited

Expect.isTrue after "Should be true"
```

The same is true of steps:

```fsharp
let before = myTestFeature.Scenarios.All.[0].Steps.[0].Visited

Expect.isFalse before "Should be false"

myTestFeature
    .Scenarios
    .``this is a scenario``
    .``0 Given this is a given step`` |> ignore

let after = myTestFeature.Scenarios.All.[0].Steps.[0].Visited

Expect.isTrue after "Should be true"
```

(There are a set of visited tests [here](https://github.com/bddkickstarter/Gherkin.TypeProvider/blob/master/tests/FSharp.Data.Gherkin.Tests/Tests/VisitedTests.tests.fs))

The entire feature can be "walked" using the underlying arrays so it is possible to find any named properties that have not been visited.

# Scenario Outlines
To help automate the *Scenario Outline* the Gherkin TypeProvider comes with the *scenarioOutline builder* .

The builder is a computational expression that allows a *Scenario Outline* to be used like a *Scenario*.  The builder will create a *Scenario* based on the outline for every row in the *Examples* table and applies the supplied function to each newly created *Scenario*.

For example, to automate the outline:

```gherkin
Feature: Outlines

Scenario Outline: some group of examples
Given some setup
When uses <uses the data>
Then result will be <checks the result>

Examples:
|uses the data|checks the result|
|    data 1   |     true!       |
|    data 2   |     false!      |
|    data 3   |     maybe?      |
```

Use the builder (with *Expecto*):

```fsharp
open Expecto
open FSharp.Data.Gherkin

type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature")>

[<Tests>]
let useBuilder =

    scenarioOutline myFeature.Scenarios.``some group of examples`` {
        return!
            fun scenario ->

                // create an Expecto test
                test scenario.Name {

                    // use the given
                    scenario.``0 Given some setup``.Text |> ignore

                    // use the when
                    let whenText = scenario.``1 When uses <uses the data>``.Text

                    // use the then
                    let thenText = scenario.``2 Then result will be <checks the result>``.Text

                    //do something - just printing the text here
                    printfn "When %s, Then %s" whenText thenText
                }

        } >>= testList

```
The builder returns a tuple of the outline name and a list of whatever was returned from the builder, in this case a list of *Expecto* tests.  The *FSharp.Data.Gherkin* namespace also includes a bind operator *(>>=*) that can bind the resulting tuple to anything that takes a name & a list (the *Expecto* **testList**) in this case

For steps that have template arguments their text is replaced with the example data so the code above would produce the output

```console
When uses data 1, Then result will be true!
When uses data 2, Then result will be false!
When uses data 3, Then result will be maybe?
```
The builder will visit each example however the steps will only be marked as visited if referenced in the builder

## xUnit
Unfortunately xUnit doesn't support this behaviour as it uses the *Theory* attribute to support running multiple tests unlike *Expecto* which can run a list of tests.

# Rules & Examples
As of Gherkin 6 Rules & Examples are now supported.  A rule is container for multiple examples (which are just synonyms for Scenarios).

So with the feature

```gherkin
Feature: Rules and Examples

    Rule: A rule is a collection of examples

        Example: An example is a scenario
            When it works the same way as a scenario
            Then they can be used to group examples

        Example: Another example for the rule
            Given the rule can have a completely different scenario
            When multiple conditions can be specified
            Then a rule can be expressed easily

    Rule: A feature can have multiple rules

        Example: A rule must have an example
            Given a scenario must have a step
```

To get the first step of the first example:

```fsharp
open FSharp.Data.Gherkin

type RulesFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/rulesAndExamples.feature")>

let feature = RulesFeature.CreateFeature()

feature
    .Rules
    .``A rule is a collection of examples``
    .``An example is a scenario``
    .``0 When it works the same way as a scenario`` |> ignore
```

# Feature Validation
To validate the feature use the FeatureValidator - shortcut in the *FSharp.Data.Gherkin* namespace:

```fsharp
open FSharp.Data.Gherkin

match validateFeature feature with
| None -> ()
| Some report -> failwith(report.Summary)
```

The validator returns a report that contains a tree of all the feature's children that have not been visited. This includes:
- Tags
- Features
- Backgrounds
- Scenarios
- Scenario Outlines
- All cells in a DataTable argument
- Every example row
- Every rule
- Every example

To exclude the feature or specific scenarios, tag them and provide the tag names as an array and the validator will exclude them from the report e.g.

```fsharp
match validateFeatureAndExclude feature ["@WIP";"@pending"] with
| None -> ()
| Some report -> failwith(report.Summary)
```
will exclude the feature if it has either of those tags, and any scenario/example that has either of the tags.

Backgrounds, Rules & Steps cannot be tagged




    







