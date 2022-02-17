module Internal.Runtime

open System
open LanguageModel


type EvalResult<'T> =
    | Ok of 'T
    | Error of string
    static member (>>=) (a, f: 'a -> EvalResult<'a>)=
        match a with
        | Error e -> Error e
        | Ok x -> f x
    static member (>=>) (a: EvalResult<'a>, f: 'a -> EvalResult<'b>) =
        match a with
        | Error e -> Error e
        | Ok x -> f x


type ProvidedFunction =
    float list -> EvalResult<float>


type EnvironmentObject =
    | Variable of float
    | CustomFunction of expectedArguments: Identifier list * functionBody: Instruction list
    | ProvidedFunction of ProvidedFunction


type Environment =
    Map<Identifier, EnvironmentObject>


type ComputationState =
    float Option * Environment


let errorPrinter msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printfn ""
    printfn "Error: %s" msg
    Console.ResetColor()


let evalResultPrinter printOnlyErrors (exp: EvalResult<'a>) =
    match exp with
    | Error e ->
        errorPrinter e
        exp
    | Ok x when not printOnlyErrors ->
        printfn "%O" x
        exp
    | _ ->
        exp

