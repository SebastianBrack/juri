module Juri.Internal.Runtime

open System
open Juri.Internal.OutputWriter
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
    IOutputWriter -> float list -> InterpreterResult<float>


and EnvironmentObject =
    | Variable of float
    | List of float array
    | CustomFunction of expectedArguments: Identifier list * functionBody: Instruction list
    | ProvidedFunction of ProvidedFunction


and Environment =
    Map<Identifier, EnvironmentObject>


and ComputationState = float Option * Environment

let evalResultPrinter
        printOnlyErrors
        (outputWriter: IOutputWriter)
        (exp: InterpreterResult<'a>) =
    match exp with
    | Error e ->
        outputWriter.WriteERR(e)
        exp
    | Ok x when not printOnlyErrors ->
        outputWriter.WriteSTD(x.ToString())
        exp
    | _ ->
        exp