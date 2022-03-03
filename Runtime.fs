module Juri.Internal.Runtime

open System
open Output
open LanguageModel


type InterpreterResult<'T> =
    | Ok of 'T
    | Error of string
    static member (>>=) (a, f: 'a -> InterpreterResult<'b>)=
        match a with
        | Error e -> Error e
        | Ok x -> f x
    static member (>=>) (f: 'a -> InterpreterResult<'b>, g: 'b -> InterpreterResult<'c>) =
        fun a ->
            match f a with
            | Error e -> Error e
            | Ok x -> g x


type ProvidedFunction =
    InterpreterOutput -> float list -> InterpreterResult<float>


and EnvironmentObject =
    | Variable of float
    | CustomFunction of expectedArguments: Identifier list * functionBody: Instruction list
    | ProvidedFunction of ProvidedFunction


and Environment =
    Map<Identifier, EnvironmentObject>


and ComputationState = float Option * Environment * InterpreterOutput


let errorPrinter msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printfn ""
    printfn "Error: %s" msg
    Console.ResetColor()


let evalResultPrinter printOnlyErrors (exp: InterpreterResult<'a>) =
    match exp with
    | Error e ->
        errorPrinter e
        exp
    | Ok x when not printOnlyErrors ->
        printfn "%O" x
        exp
    | _ ->
        exp

