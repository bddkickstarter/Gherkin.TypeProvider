# Gherkin.TypeProvider
## Type provider for the Gherkin language

The Gherkin TypeProvider allows access to the data in Gherkin feature files by generating an type system representing the feature file supplied as an argument

### Background

The [Gherkin](https://cucumber.io/docs/gherkin/reference/) language provides a strict schema that has an existing [parser](https://www.nuget.org/packages/gherkin/) which is used by the major Gherkin parsing automation tools such as Specflow or the Cucumber family.

This is used by the Gherkin typeprovider to parse the supplied feature file to expose the Gherkin abstract syntax tree.  

* A Feature has an array of children, that can be a Background, Scenario or Scenario Outline.
* A child has an array of steps which have various properties such as keyword and text
* A step can optionally have an argument which can either be a document string or a data table
* A scenario can have an optional array of examples
* Features & scenarios can have an optinal array of tags

The Gherkin Type Provider simply adds to this structure by creating named properties for important elements in the Feature.  Those elements are:

* Scenario & Scenario Outline names
* Step keyword, order & text
* Tag names
* Data in either DocString/DataTable step arguments, or Example tables

So by referencing the named properties for these elements, if the feature file changes, then the property names will change and code referencing the old property names will now fail.

All of the properties that tie the test to the feature also have a Visited tag that is only set when the scenario, step, tag etc is accessed through the names property.  

This allows the test to checked after a test run to ensure all the significant parts of the feature file have been consumed in the test

## Usage

### **Note:** **Nuget package not currently available.  This will be the usage when available**

* ### Add a reference package Gherkin.TypeProvider

    Either nuget or paket whichever is easiet
* ### Reference the FSharp.Data.Gherkin namepsace
    
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

* ### Bask in the glory of a Gherkin type system

    # Scenarios & Steps

    If we had the following feature

    ```gherkin
    Feature: a feature

    Scenario: this is a scenario

        Given this is a given step

    ```

    Then the Gherkin Typeprovider would expose that as the following

    ```fsharp
        myTestFeature.``this is a scenario``.``0 Given this is a given step``
    ```

    The object returned by the step has the information from the feature file so

    ```fsharp
        let step = myTestFeature.``this is a scenario``.``0 Given this is a given step``

        printfn "Order: %i, Keyword: %s, Text: %s" step.Order step.Keyword step.Text
    ```

    Would print out

    ```console
    Order: 0, Keyword: Given, Text: this is a given step
    ```

    # Step Arguments
    Steps can have doc string arguments, so

    ```gherkin
    Feature: a feature

    Scenario: this is a scenario

        Given this is a given step
        When a when step with a string argument
        """
            this is a muti
            line string argument
        """
    ```

    Would be represented as
    
    ```
    myTestFeature
        .``this is a scenario``
        .``1 a when step with a string argument``
        .Argument
        .Content
    ```

    The **Argument** in this case is of type _DocString_ so returns an objet that has the _Content_ and _ContentType_ properties containing the values from the feature file, so would print out

    ```console
    this is a muti
    line string argument
    ```

    But an argument can also be a data table, what then? 
    
    As the type system is generated, an argument on a specific senario can be any type we please.  
    If a step has a string argument it is accessed as above, but a step that has a data table argument is slightly different.

    In this feature

    ```gherkin
    Feature: a feature

    Scenario: this is a scenario

        Given this is a given step
        When a when step with a string argument
        """
            this is a muti
            line string argumnent
        """
        Then a then step with a data table argument
        |Column 1| Column 2|
        | Data 1 | Data 2  |
        | Data 3 | Data 4  |
    
    ```

    Here the **Argument** is no longer a _DocString_,but instead an array of data objects that have individual data cells as named properties.

    ```
    myTestFeature
        .``this is a scenario``
        .``2 a then step with a data table argument``
        .Argument
        .[1]
        .``Column 2``
        .Value
    ```

    Which would evaluate to

    ```console
    Data 4
    ```

    So accessing the second row of the argument we select the second row of the data table.  
    
    Then by using the named column "Column 2", the data cell for the 2nd row for & 2nd column is returned.

    A _DataCell_ has 2 properties; Header & Value.  The header contains the name of the cell's column, while the value is the contents of the cell

    
    # Scenario Outlines & Examples

    Examples are just a data table and accessed in a similar way to the data table argument.

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

    The *data 2* cell's data can be accessed using

    ```
    myTestFeature
        .``some group of examples``
        .Examples
        .[0]
        .``checks the result``
        .Value
    ```

    Which would evaluate to

    ```console
    true!
    ```

    







