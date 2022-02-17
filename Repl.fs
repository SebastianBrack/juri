module Internal.Repl

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
    //printfn "user input: %A" (userInput.ToCharArray())
    match parseProgram userInput with
    | Success (r,_,_) ->
        compute r state
        |> evalResultPrinter true
        >>= repl
    | Failure (_) ->
        repl state
    | Fatal (_) ->
        repl state



let startRepl () =
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
    repl initialState