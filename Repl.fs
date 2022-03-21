module Juri.Internal.Repl

open Interpreter
open CoreLib
open Juri.Internal.OutputWriter
open Juri.Internal.Parser
open Juri.Internal.Runtime
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
    let stream = CharStream(userInput, JuriContext.Default)
    match stream.RunParser(juriProgram) with
    | Success (r,_,_) ->
        compute r outputWriter state
        >>= repl outputWriter
    | Failure (msg, _) ->
        outputWriter.WriteERR(msg, stream.GetContext().Line)
        repl outputWriter state
    | Fatal (msg, _) ->
        outputWriter.WriteERR(msg, stream.GetContext().Line)
        repl outputWriter state



let startRepl () =
    let outputWriter = ConsoleWriter()
    let initialState = { ComputationState.Default with Environment = createEnvWithCoreLibFunctions () }
    repl outputWriter initialState