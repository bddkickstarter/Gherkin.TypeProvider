module ExpressionBuilders.Scenario

open System.Reflection

open ObjectModel
open ExpressionBuilders.TagContainer
open ExpressionBuilders.Data
open ExpressionBuilders.Step
open BaseTypes.Step
open Shared

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

open Gherkin.Ast

type ScenarioExpressionBuilder 
        (
        tagClassBaseType:System.Type,
        tagBaseType:System.Type,
        tagExpressionBuilder:TagContainerExpressionBuilder,
        dataRowBaseType:System.Type,
        dataExpressionBuilder:DataExpressionBuilder,
        scenarioBaseType:System.Type,
        stepExpressionBuilder:StepExpressionBuilder,
        stepBaseType:System.Type,
        propertySanitizer:string->string) =

    member __.CreateExpression (parent:ProvidedTypeDefinition) (gherkinScenario:Scenario) =
        let scenarioType = ProvidedTypeDefinition((sprintf "%sClass" gherkinScenario.Name) |> Sanitizer().Sanitize ,Some scenarioBaseType,isErased=false, hideObjectMethods=true, isSealed=false)
        scenarioType |> parent.AddMember

        //create tags
        let tagExpression = tagExpressionBuilder.CreateExpression scenarioType (gherkinScenario.Tags |> Seq.toList |> List.map(fun t -> t.Name))

        //get the examples if any
        let exampleExpression = 
            match gherkinScenario.Examples |> Seq.toList with
            | [] -> None
            | examples -> 
                let columns = examples.[0].TableHeader.Cells |> Seq.map (fun c -> c.Value) |> Seq.toList
                let exampleType  = dataExpressionBuilder.CreateExpression scenarioType columns
                let exampleField = PropertyHelper(scenarioType).AddProperty("Examples",exampleType.MakeArrayType())
                Some (exampleType,exampleField)

        //add step specific constructor params, properties & fields
        let gherkinStepList = gherkinScenario.Steps |> Seq.toList
        let stepExpressions =  gherkinStepList |> List.mapi((stepExpressionBuilder.CreateExpression scenarioType))

        let parameters = stepExpressions |> List.mapi(fun i (stepExpression:StepExpression) -> ProvidedParameter(sprintf "step%i" i ,stepExpression.Type))  
        let stepFields = stepExpressions |> List.mapi(fun i (stepExpression:StepExpression) -> ProvidedField(sprintf "_step%i" i ,stepExpression.Type))

        let visitedProperty = stepBaseType.GetProperty("Visited")
        let outlineProperty = scenarioBaseType.GetProperty("Outline")
        let parentStepsProperty = scenarioBaseType.GetProperty("Steps")
        let getValue =
                typeof<System.Array>
                    .GetMethods(BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.Instance)
                    |> Seq.pick(fun m ->
                        let parameters = m.GetParameters()
                        if m.Name = "GetValue" && parameters.Length = 1 && typeof<int>.IsAssignableFrom(parameters.[0].ParameterType)
                        then Some m else None)

        let stepProperties = 
            List.mapi2(
                fun i step (stepExpression:StepExpression) ->
                    
                    ProvidedProperty(
                        StepBase.GetStepName(propertySanitizer,i,step),
                        stepExpression.Type,
                        getterCode=fun args-> 
                            //get the specific step field
                            let getStep = Expr.FieldGet(args.[0],stepFields.[i])
   
                            //set visited property of parent to true
                            let parent = Expr.PropertyGet(args.[0],outlineProperty)
                            let parentSteps = Expr.PropertyGet(parent,parentStepsProperty)
                            let stepsAsObjArray = Expr.Coerce(parentSteps, typeof<System.Array>)
                            let getOutlineStep = Expr.Call(stepsAsObjArray,getValue,[Expr.Value(i)])
                            let getParentStepAsStepBase = Expr.Coerce(getOutlineStep,stepBaseType)
                            let visitParentStep = Expr.PropertySet(getParentStepAsStepBase,visitedProperty,Expr.Value(true))

                            //visit the parent if it exists
                            let visitStep = Expr.PropertySet(getStep,visitedProperty,Expr.Value(true))
                            let visitParentThenStep = Expr.Sequential(visitParentStep,visitStep)
            
                            // check to see if parent is null
                            let parentAsObject = Expr.Coerce(parent,typeof<obj>)
                            let guard = <@@ not (isNull %%parentAsObject) @@>
                            
                            Expr.IfThenElse(guard,Expr.Sequential(visitParentThenStep,getStep),Expr.Sequential(visitStep,getStep))

                            )) gherkinStepList stepExpressions 

        stepFields |> Seq.iter(scenarioType.AddMember)
        stepProperties |> Seq.iter(scenarioType.AddMember)

        let constructorParams =
            let mandatory  = [ProvidedParameter("outline",scenarioBaseType) ; ProvidedParameter("name",typeof<string>) ; ProvidedParameter("description",typeof<string>)]
            match exampleExpression,tagExpression with
            | None,None -> mandatory @ parameters
            | Some (exampleType,_), None -> 
                mandatory @ ([ProvidedParameter("examples",exampleType.MakeArrayType())]) @  parameters
            | None, Some(tagType,_) ->
               mandatory @ [ProvidedParameter("tags",tagType)] @ parameters
            | Some (exampleType,_),Some(tagType,_) ->
               mandatory @ [ProvidedParameter("tags",tagType)] @ [ProvidedParameter("examples",exampleType.MakeArrayType())] @ parameters

        let getStepsFromArgs (args:Expr list) examples tags =
                //get the steps from arguments based on whether there are examples & or tags
                match examples,tags with
                | None,None -> args.GetSlice(Some 4,Some (args.Length-1))
                | Some(_),None ->args.GetSlice(Some 5,Some (args.Length-1))
                | Some(_),Some(_) ->args.GetSlice(Some 6,Some (args.Length-1))
                | None,Some(_) -> args.GetSlice(Some 5,Some (args.Length-1))

        let scenarioCtr = 
            ProvidedConstructor(
                constructorParams,
                invokeCode = 
                    fun args -> 
                            let this = args.[0]

                            let steps = getStepsFromArgs args exampleExpression tagExpression

                            //set each parameter to its non-derived backing field
                            let stepFieldSets = List.map2( fun stepField stepValue -> Expr.FieldSet(this,stepField,stepValue))  stepFields steps

                            //create a single expression with all the step sets & the new array
                            let stepFieldSet = stepFieldSets.Tail |> Seq.fold (fun a c -> Expr.Sequential(a,c) ) stepFieldSets.Head
                            
                            // add any background and tags
                            let additionalSets =
                                match exampleExpression,tagExpression with
                                | None,None -> []
                                | Some(_,exampleField),None -> [Expr.FieldSet(this,exampleField,args.[4])]
                                | Some(_,exampleField),Some(_,tagField) -> [Expr.FieldSet(this,exampleField,args.[5]);Expr.FieldSet(this,tagField,args.[4])]
                                | None,Some(_,tagField) ->  [Expr.FieldSet(this,tagField,args.[4])]

                            additionalSets |> Seq.fold(fun a c -> Expr.Sequential(a,c)) stepFieldSet)

        // override base constructor 
        let baseCtr = scenarioBaseType.GetConstructors().[0]

        scenarioCtr.BaseConstructorCall <- 
            fun args -> 
                let steps = 
                    getStepsFromArgs args exampleExpression tagExpression
                    |> List.map(fun s -> Expr.Coerce(s,stepBaseType))
                    
                let stepsArray = Expr.NewArray(stepBaseType,steps)

                let (tags,examples) =
                    let emptyExamples = Expr.NewArray(dataRowBaseType,[])
                    let emptyTags= Expr.NewObject(tagClassBaseType.GetConstructors().[0],[Expr.NewArray(tagBaseType,[])])
                    match tagExpression,exampleExpression with
                    | None,None -> emptyTags,emptyExamples
                    | Some _,None -> args.[4],emptyExamples
                    | Some _,Some _ -> args.[4],args.[5]
                    | None, Some _ -> emptyTags,args.[4]
                
                baseCtr,[args.[0];args.[1];args.[2];args.[3];tags;examples;stepsArray] 

        scenarioCtr |> scenarioType.AddMember
        
        {
            Name = gherkinScenario.Name
            Type = scenarioType
            Steps = stepExpressions
            Examples = match exampleExpression with | None -> None | Some(exampleExpr,_) -> Some exampleExpr
            Tags = match tagExpression with | None -> None | Some(tagExpr,_) -> Some tagExpr
        }
        
    static member CreateNew (providerModel:GherkinProviderModel) (propertyNameSanitizer:string->string) =

        ScenarioExpressionBuilder(
                                    providerModel.TagContainerBaseType,
                                    providerModel.TagBaseType,
                                    TagContainerExpressionBuilder.CreateNew providerModel,
                                    providerModel.DataRowBaseType,
                                    DataExpressionBuilder.CreateNew providerModel propertyNameSanitizer,
                                    providerModel.ScenarioBaseType,
                                    StepExpressionBuilder.CreateNew providerModel propertyNameSanitizer,
                                    providerModel.StepBaseType,
                                    propertyNameSanitizer)

        

