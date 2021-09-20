module CoreLib

open Runtime
open LanguageModel

let private buildinAdd : ProvidedFunction = List.reduce ( + ) >> Ok
let private buildinMul : ProvidedFunction = List.reduce ( * ) >> Ok
let private buildinSub : ProvidedFunction = List.reduce ( - ) >> Ok
let private buildinDiv : ProvidedFunction = List.reduce ( / ) >> Ok

let private buildinEquals : ProvidedFunction =
    fun args ->
        match args with
        | [] | [_] -> Error (sprintf "Diese Funktion erwartet mindestens 2 Argumente - es wurden aber %i übergeben" args.Length)
        | head :: tail ->
            if List.forall ((=)head) tail
                then Ok 1.
                else Ok 0.

let private buildinInBoundarys : ProvidedFunction =
    fun args ->
        match args with
        | [a;b]   -> if a < b then Ok 1. else Ok 0.
        | [a;b;c] -> if a <= b && b < c then Ok 1. else Ok 0.
        | _       -> Error (sprintf "Diese Funktion erwartet 2 oder 3 Argumente - es wurden aber %i übergeben." args.Length)

let private buildinPrint : ProvidedFunction =
    fun args ->
        match args with
        | [x] -> printfn "%A" x
        | _   -> printfn "%A" args
        Ok 0.


let crateEnvWithCoreLibFunctions () : Environment =
    Map [
        (Identifier "add", ProvidedFunction buildinAdd)
        (Identifier "mul", ProvidedFunction buildinMul)
        (Identifier "sub", ProvidedFunction buildinSub)
        (Identifier "div", ProvidedFunction buildinDiv)
        (Identifier "bnd", ProvidedFunction buildinInBoundarys)
        (Identifier "eq", ProvidedFunction buildinEquals)
        (Identifier "print", ProvidedFunction buildinPrint)
        ]