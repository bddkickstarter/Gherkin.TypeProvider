namespace Shared

open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type Sanitizer(?sanitizeType:string) =
    let numbers = [48..57]
    let lowercaseChars = [97..122]
    let uppercaseChars = [65..90]
    let allowedFSharpCharacters = [32]
    let illegalFSharpChars = [| '.'; '+'; '$'; '&'; '['; ']'; '/'; '\\'; '*'; '\"'; '`' |]

    let sanitizeFirstNumber  (str:string) =
            match (str.ToCharArray() |> Seq.toList) with
            | n :: _ when System.Char.IsNumber(n) -> sprintf "_%s" str
            | _ -> str

    let removeIllegalFSharpChars (str:string) = 
             str.ToCharArray()
             |> Array.map(fun c -> if illegalFSharpChars |> Seq.exists(fun ic -> ic = c) then '_' else c)
             |> System.String

    let allowCharacters  (validCharacters:int list)  (str:string) =
            str.ToCharArray() 
            |> Array.map(fun c -> 
                    let asciiCode = (c |> int) 
                    if validCharacters |> Seq.exists(fun a -> a = asciiCode) then c
                    else '_')

    let sanitizeByType (nm:string) =
        match sanitizeType with
        | None | Some "full" ->
            nm 
            |> sanitizeFirstNumber
            |> allowCharacters (numbers @ lowercaseChars @ uppercaseChars)
            |> System.String
        | Some "partial" -> 
            nm 
            |> allowCharacters (allowedFSharpCharacters @ numbers @ lowercaseChars @ uppercaseChars)
            |> System.String
        | _ -> nm |> removeIllegalFSharpChars

    member __.Sanitize(str:string) = sanitizeByType str

type PropertyHelper (parent:ProvidedTypeDefinition) =

    member __.AddProperty (name:string,propertyType:System.Type) =
        let fieldName = (sprintf "_%s" (name.ToLower())) |> Sanitizer().Sanitize
        let field = ProvidedField(fieldName,propertyType)
        let property = 
            ProvidedProperty(
                name,propertyType,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],field)))

        field |> parent.AddMember
        property |> parent.AddMember

        field

    member __.AddVisitedProperty() =
        let visitedField = ProvidedField("_visited",typeof<bool>)
        let visitedProperty = 
            ProvidedProperty(
                "Visited",typeof<bool>,isStatic=false,
                getterCode = (fun args -> Expr.FieldGet(args.[0],visitedField)),
                setterCode = (fun args -> Expr.FieldSet(args.[0],visitedField,args.[1])))

        visitedField |> parent.AddMember
        visitedProperty |> parent.AddMember

        visitedField 