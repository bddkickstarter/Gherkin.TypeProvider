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
open FSharp.Quotations

type GherkinProviderModel (providerName:string,root:ProvidedTypeDefinition) as this=

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
    let allTagsProperty = tagContainerBase.Type.GetProperty("AllTags")
    let arrayType = typeof<System.Array>
    let lengthProperty = arrayType.GetProperty("Length")
    let tagVisitedProperty = tagBase.Type.GetProperty("Visited")
    let tagNameProperty = tagBase.Type.GetProperty("Name")


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

    member private __.AddHasTagsMethod (parent:ProvidedTypeDefinition) (getTags:Expr->Expr) =
            
            let hasTagMethod =
                    ProvidedMethod(
                        "HasTag",
                        [ProvidedParameter("tagName",typeof<string>)],
                        typeof<bool>,
                        isStatic = false,
                        invokeCode = fun args ->
                            let this = args.[0]
                            let foundTag = Var("foundTag",typeof<bool>,isMutable=true)
                            let tag = Var("tag",tagBase.Type,isMutable=false)

                            let visitTag = 
                                    Expr.Sequential(
                                        Expr.PropertySet(Expr.Var(tag),tagVisitedProperty,Expr.Value(true)),
                                        Expr.Sequential(Expr.VarSet(foundTag,Expr.Value(true)),Expr.Value(true)))

                            let guard = 
                                let tagExpr = Expr.Coerce(Expr.Var(tag),typeof<obj>)
                                let expectedTagName = args.[1]
                                let actualTagName = Expr.PropertyGet(Expr.Var(tag),tagNameProperty)
                                <@@
                                    let tag :obj = %%tagExpr
                                    let actualName :string= %%actualTagName 
                                    let expectedName :string= %%expectedTagName 
                                    not (isNull tag)  && (actualName = expectedName)
                                @@>

                            let ifTagFound = 
                                    Expr.IfThenElse(guard,visitTag,Expr.Value(false))

                            let allTagsAsArray = 
                                Expr.Coerce
                                    (Expr.PropertyGet(getTags this,allTagsProperty),arrayType)

                            let allTagsLength = Expr.PropertyGet(allTagsAsArray,lengthProperty)

                            let getTagAsObject =
                                    let loopVarExpr = Expr<int>.GlobalVar("i")
                                    Expr.Coerce(
                                        <@@
                                            let arr :System.Array= %%allTagsAsArray
                                            let index :int= %loopVarExpr
                                            arr.GetValue(index)
                                        @@>,tagBase.Type)
                            
                            let getTagByIndexAsTagBase = 
                                Expr.Let(tag,getTagAsObject,ifTagFound)

                            let finishLoop =
                                <@@ 
                                    let l :int = %%allTagsLength
                                    l-1
                                @@>

                            Expr.Sequential(
                                Expr.Let(foundTag,Expr.Value(false),
                                    Expr.ForIntegerRangeLoop(Var.Global("i",typeof<int>),Expr.Value(0),finishLoop,getTagByIndexAsTagBase)),
                                Expr.Var(foundTag))
                    )
                    
            hasTagMethod |> parent.AddMember 

    member __.AddHasTagMethodWithField (parent:ProvidedTypeDefinition)  (tagsField:ProvidedField) =
        this.AddHasTagsMethod parent (fun this -> Expr.FieldGet(this,tagsField))

    member __.AddHasTagMethodWithProperty (parent:ProvidedTypeDefinition) (tagsProperty:System.Reflection.PropertyInfo) =
        this.AddHasTagsMethod parent (fun this -> Expr.PropertyGet(this,tagsProperty))