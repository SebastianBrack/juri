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

let map f = function
    | Ok r -> Ok (f r)
    | Error msg -> Error msg

type ProvidedFunction =
    IOutputWriter -> float list -> InterpreterResult<float>


and EnvironmentObject =
    | Variable of float
    | List of float array
    | CustomFunction of expectedArguments: Identifier list * functionBody: JuriProgram
    | ProvidedFunction of ProvidedFunction


and Environment =
    Map<Identifier, EnvironmentObject>


and ComputationState =
    {
        LastExpression : float option
        BreakFlag : bool
        ReturnFlag : bool
        Environment : Environment
    }
    static member Default = {
        LastExpression = None
        BreakFlag = false
        ReturnFlag = false
        Environment = Map.empty }

let evalResultPrinter
        printOnlyErrors
        (outputWriter: IOutputWriter)
        (exp: InterpreterResult<'a>) =
    match exp with
    | Error e ->
        outputWriter.WriteERR(e, -1)
        exp
    | Ok x when not printOnlyErrors ->
        outputWriter.WriteSTD(x.ToString())
        exp
    | _ ->
        exp