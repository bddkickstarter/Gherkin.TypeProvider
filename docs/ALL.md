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
        .``this is a scenario``
        .``0 Given this is a given step``
```

The object returned by the named step has the information from the feature file so,

```fsharp
    let step = 
        myTestFeature
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
Feature: outlines

Scenario Outline: some group of examples
When outline when <uses the data>
Then outline then <checks the result>

Examples:
|uses the data|checks the result|
|    data 1   |     true!       |
```

The data from the second column of the first row would be accessed using:

```fsharp
myTestFeature
    .``some group of examples``
    .Examples
    .[0]
    .``checks the result``
    .Value
```

And would evaluate to:

```console
true!
```

# Consuming from C#

To consume the type system in a C# project create an F# project to host the types.

As C# won't recognised properties with illegal characters in them (such as spaces)  santize the names by using the Santize option:

```fsharp
type TestFeature = GherkinProvider<const(__SOURCE_DIRECTORY__ + "/test.feature"), Sanitize=true>
```

Which will replace illegal characters with underscores.  The previous example now looks like this:

```csharp
myTestFeature
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
        .``this is a scenario``
```

The type of the scenario will be 

```fsharp
``this is a scenario Class`` 
```

However the feature also contains an array of all its Scenarios as their base classes (_TestFeature_ScenarioBase_), so if the only scenario in this feature was accessed like:

```fsharp
let scenario = 
    myTestFeature
        .Scenarios.[0]
```

The type of the scenario will be 

```fsharp
``TestFeature_ScenarioBase`` 
```

The base class has the _Visited_ property which is set to true __only__ when the named property is used, so the following test will pass:

```fsharp
let before = myTestFeature.Scenarios.[0].Visited

Expect.isFalse before "Should be false"

myTestFeature.``this is a scenario`` |> ignore

let after = myTestFeature.Scenarios.[0].Visited

Expect.isTrue after "Should be true"
```

The same is true of steps:

```fsharp
let before = myTestFeature.Scenarios.[0].Steps.[0].Visited

Expect.isFalse before "Should be false"

myTestFeature
    .``this is a scenario``
    .``this is a given step`` |> ignore

let after = myTestFeature.Scenarios.[0].Steps.[0].Visited

Expect.isTrue after "Should be true"
```

(There are a set of visited tests [here](https://github.com/bddkickstarter/Gherkin.TypeProvider/blob/master/tests/FSharp.Data.Gherkin.Tests/Tests/VisitedTests.tests.fs))

The entire feature can be "walked" using the underlying arrays so it is possible to find any named properties that have not been visited.


    







