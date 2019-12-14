namespace ObjectModel

open ProviderImplementation.ProvidedTypes

type ArgumentExpression =
| DocStringType of ProvidedTypeDefinition
| DataTableType of ProvidedTypeDefinition

type StepExpression = 
    {
        Name:string
        Type:ProvidedTypeDefinition
        Position:int
        Argument:ArgumentExpression option
    }

type ScenarioExpression =
    {
        Name:string
        Type:ProvidedTypeDefinition
        Steps:StepExpression list
        Examples:ProvidedTypeDefinition option
        Tags:ProvidedTypeDefinition option
    }

type BackgroundExpression =
    {
        Type:ProvidedTypeDefinition
        Steps:StepExpression list
    }

type FeatureExpression =
    {
        Name:string
        Type:ProvidedTypeDefinition
        Scenarios:ScenarioExpression list
        Background:BackgroundExpression option
        Tags:ProvidedTypeDefinition option
    }

open Shared
open BaseTypes.Argument
open BaseTypes.DataCell
open BaseTypes.DataRow
open BaseTypes.Tag
open BaseTypes.DocString
open BaseTypes.Step
open BaseTypes.Scenario

type GherkinProviderModel (providerName:string,root:ProvidedTypeDefinition,sanitizeType:string) =

    let argumentBase = ArgumentBase(providerName,root)
    let dataCellType = DataCellBase(argumentBase,providerName,root)
    let dataRowBase = DataRowBase(dataCellType,providerName,root)
    let tagBase = TagBase(providerName,root)
    let docStringBase = DocStringArgumentBase(argumentBase,providerName,root)
    let stepBase = StepBase(argumentBase,dataRowBase,providerName,root)
    let scenarioBase = ScenarioBase(stepBase,providerName,root)

    member val StepBaseType = stepBase.Type with get
    member val ScenarioBaseType = scenarioBase.Type with get
    member val TagBaseType = tagBase.Type with get
    member val DocStringArgType = docStringBase.Type with get
    member val DataRowBaseType = dataRowBase.Type with get
    member val DataCellBaseType = dataCellType.Type with get
    member val ArgumentBaseType = argumentBase.Type with get
    member __.SanitizeProperty = Sanitizer(sanitizeType).Sanitize