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
    
type RuleExpression =
    {
        Name:string
        Type:ProvidedTypeDefinition
        Examples:ScenarioExpression list
    }
 
    
type RuleContainerExpression =
    {
        Type:ProvidedTypeDefinition
        Rules:RuleExpression list
    }

type ScenarioContainerExpression =
    {
        Type:ProvidedTypeDefinition
        Scenarios:ScenarioExpression list
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
        Scenarios:ScenarioContainerExpression
        Rules:RuleContainerExpression
        Background:BackgroundExpression option
        Tags:ProvidedTypeDefinition option
    }

open BaseTypes.Argument
open BaseTypes.DataCell
open BaseTypes.DataRow
open BaseTypes.Tag
open BaseTypes.TagContainer
open BaseTypes.DocString
open BaseTypes.Step
open BaseTypes.Scenario
open BaseTypes.ScenarioContainer
open BaseTypes.Rule
open BaseTypes.RuleContainer

type GherkinProviderModel (providerName:string,root:ProvidedTypeDefinition) =

    let argumentBase = ArgumentBase(providerName,root)
    let dataCellType = DataCellBase(argumentBase,providerName,root)
    let dataRowBase = DataRowBase(dataCellType,providerName,root)
    let tagBase = TagBase(providerName,root)
    let tagContainerBase = TagContainerBase(tagBase,providerName,root)
    let docStringBase = DocStringArgumentBase(argumentBase,providerName,root)
    let stepBase = StepBase(argumentBase,dataRowBase,providerName,root)
    let scenarioBase = ScenarioBase(tagContainerBase,stepBase,dataRowBase,providerName,root)
    let scenarioContainerBase = ScenarioContainerBase(scenarioBase,providerName,root)
    let ruleBase = RuleBase(scenarioBase,providerName,root)
    let ruleContainerBase = RuleContainerBase(ruleBase,providerName,root)

    member val StepBaseType = stepBase.Type with get
    member val ScenarioBaseType = scenarioBase.Type with get
    member val ScenarioContainerBaseType = scenarioContainerBase.Type with get
    member val TagBaseType = tagBase.Type with get
    member val TagContainerBaseType = tagContainerBase.Type with get
    member val DocStringArgType = docStringBase.Type with get
    member val DataRowBaseType = dataRowBase.Type with get
    member val DataCellBaseType = dataCellType.Type with get
    member val ArgumentBaseType = argumentBase.Type with get
    member val RuleContainerBaseType = ruleContainerBase.Type with get
    member val RuleBaseType = ruleBase.Type with get