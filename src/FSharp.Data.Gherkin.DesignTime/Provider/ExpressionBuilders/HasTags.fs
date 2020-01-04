module ExpressionBuilders.HasTags
open ObjectModel
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open System.Reflection

type HasTagsMethodExpressionBuilder (providerModel:GherkinProviderModel) =

    let getAllTagsProperty = providerModel.TagContainerBaseType.GetProperty("AllTags")
    let arrayType = typeof<System.Array>
    let getAllTagsLengthProperty = arrayType.GetProperty("Length")
    let tagVisitedProperty = providerModel.TagBaseType.GetProperty("Visited")

    let getValue =
        arrayType
            .GetMethods(BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.Instance)
            |> Seq.pick(fun m ->
                let parameters = m.GetParameters()
                if m.Name = "GetValue" && parameters.Length = 1 && typeof<int>.IsAssignableFrom(parameters.[0].ParameterType)
                then Some m else None)

    member __.AddHasTagsMethod (parent:ProvidedTypeDefinition) (tagsField) =
            
            let hasTagMethod =
                    ProvidedMethod(
                        "HasTag",
                        [ProvidedParameter("tagName",typeof<string>)],
                        typeof<bool>,
                        isStatic = false,
                        invokeCode = fun args ->
                            let this = args.[0]
                            let loopVarExpr = Expr<int>.GlobalVar("i")
                            
                            let getTags = Expr.FieldGet(this,tagsField)
                            let allTags = Expr.PropertyGet(getTags,getAllTagsProperty)
                            let allTagsAsArray = Expr.Coerce(allTags,arrayType)
                            let allTagsLength = Expr.PropertyGet(allTagsAsArray,getAllTagsLengthProperty)
                            let foundTag = Var("foundTag",typeof<bool>,isMutable=true)

                            let guard = 
                                <@@
                                    let arr :System.Array = %%allTagsAsArray
                                    let index :int= %loopVarExpr
                                    not (isNull (arr.GetValue(index)))
                                @@>

                            let getTagByIndexAsTagBase = 
                                Expr.Coerce(
                                    <@@
                                        let arr :System.Array = %%allTagsAsArray
                                        let index :int= %loopVarExpr
                                        arr.GetValue(index)
                                    @@>, providerModel.TagBaseType)

                            let visitTag = 
                                    Expr.Sequential(
                                        Expr.PropertySet(getTagByIndexAsTagBase,tagVisitedProperty,Expr.Value(true)),
                                        Expr.Sequential(Expr.VarSet(foundTag,Expr.Value(true)),Expr.Value(true)))

                            let ifTagFound = 
                                    Expr.IfThenElse(guard,visitTag,Expr.Value(false))

                            let finishLoop =
                                <@@ 
                                    let l :int = %%allTagsLength
                                    l-1
                                @@>

                            Expr.Sequential(
                                Expr.Let(foundTag,Expr.Value(false),Expr.ForIntegerRangeLoop(Var.Global("i",typeof<int>),Expr.Value(0),finishLoop,ifTagFound)),
                                Expr.Var(foundTag)) //!!!
                    )
                    
            hasTagMethod |> parent.AddMember 

    static member CreateNew (providerModel:GherkinProviderModel) =
        HasTagsMethodExpressionBuilder(providerModel)