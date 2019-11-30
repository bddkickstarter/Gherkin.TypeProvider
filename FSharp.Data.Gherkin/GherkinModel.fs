namespace FSharp.Data.Gherkin

type DataCell (header:string,value:string) =
    member __.Header = header
    member __.Value = value

type DataRow (data:seq<DataCell>) = 
    member __.Data = data

type Step (order:int,keyword:string,text:string) =
    member __.Order = order
    member __.Text = text    
    member __.Keyword = keyword   

type Background (name:string,description:string,steps:seq<Step>) =
    member __.Name = name
    member __.Description = description
    member __.Steps = steps

type Scenario (name:string,description:string,steps:seq<Step>,examples:seq<DataRow>) =
    member __.Name = name
    member __.Description = description
    member __.Steps = steps
    member __.Examples = examples

type UnTyped (name:string,description:string,scenarios:seq<Scenario>,scenarioOutlines:seq<Scenario>,background:Background) =
    member __.Background = background
    member __.Scenarios = scenarios
    member __.ScenarioOutlines = scenarioOutlines
    member __.Name = name
    member __.Description = description

type Feature (name:string,description:string,scenarios:seq<Scenario>,scenarioOutlines:seq<Scenario>,background:Background) =
    member __.Name = name
    member __.Description = description
    member __.Feature = UnTyped(name,description,scenarios,scenarioOutlines,background)



module Constructors =
    let Feature = (typeof<Feature>).GetConstructors().[0]
    let Background = (typeof<Background>).GetConstructors().[0]
    let Scenario = (typeof<Scenario>).GetConstructors().[0]
    let Step = (typeof<Step>).GetConstructors().[0]
    let DataCell = (typeof<DataCell>).GetConstructors().[0]