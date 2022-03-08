module Juri.Internal.Repl

open Interpreter
open CoreLib
open Juri.Internal.OutputWriter
open Runtime
open Parser
open ParserCombinators



let rec private repl (outputWriter: IOutputWriter) (state: ComputationState) =
    let mutable userInput = ""
    let mutable line = stdin.ReadLine()
    while not (line.EndsWith(";")) do
        userInput <- userInput + line + "\n"
        line <- stdin.ReadLine()
    userInput <- userInput + line.TrimEnd(';') + "\n"
    match parseProgram userInput with
    | Success (r,_,_) ->
        compute r outputWriter state
        |> evalResultPrinter true outputWriter
        >>= repl outputWriter
    | Failure (msg, _) ->
        outputWriter.WriteERR(msg+"\n")
        repl outputWriter state
    | Fatal (msg, _) ->
        outputWriter.WriteERR(msg+"\n")
        repl outputWriter state



let startRepl () =
    let outputWriter = ConsoleWriter()
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
    repl outputWriter initialState