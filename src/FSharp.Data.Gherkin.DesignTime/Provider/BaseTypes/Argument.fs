namespace BaseTypes.Argument

open Shared
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type ArgumentBase (parentName:string,parent:ProvidedTypeDefinition) =

    let baseType =
        let baseName = sprintf "%s_ArgumentBase" parentName |> Sanitizer().Sanitize 
        let docArgumentBase = ProvidedTypeDefinition(baseName,Some typeof<obj>,isErased=false, isSealed=false, hideObjectMethods=true)
        docArgumentBase |> parent.AddMember

        let visitedField = PropertyHelper(docArgumentBase).AddVisitedProperty()

        ProvidedConstructor([],invokeCode = fun args -> Expr.FieldSet(args.[0],visitedField,Expr.Value(false))) 
        |> docArgumentBase.AddMember

        docArgumentBase
    
    member val Type = baseType with get 