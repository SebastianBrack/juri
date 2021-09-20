module Repl

open Interpreter
open CoreLib
open Runtime
open Parser
open ParserCombinators



let rec private repl (state: ComputationState) =
    let mutable userInput = ""
    let mutable line = stdin.ReadLine()
    while not (line.EndsWith(";")) do
        userInput <- userInput + line + "\n"
        line <- stdin.ReadLine()
    userInput <- userInput + line.TrimEnd(';') + "\n"
    match parseProgramm userInput with
    | Result.Ok presult ->
        compute presult.Result state
        |> evalResultPrinter true
        >>= repl
    | Result.Error e ->
        parserErrorPrinter e
        repl state



let startRepl () =
    let initialState : ComputationState = (None, crateEnvWithCoreLibFunctions())
    repl initialState