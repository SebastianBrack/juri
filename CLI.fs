module Juri.Internal.CLI

open Juri.Internal.OutputWriter
open Repl
open Parser
open ParserCombinators
open System.IO
open Interpreter
open CoreLib
open Runtime

let private runScript script =
    let outputWriter : IOutputWriter = ConsoleWriter()
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
    match parseProgram (script+"\n") with
    | Success (r,_,_) -> 
        compute r outputWriter initialState
        |> evalResultPrinter false outputWriter
        |> ignore
        0
    | Failure (msg, _) ->
        outputWriter.WriteERR(msg)
        0
    | Fatal (msg, _) ->
        outputWriter.WriteERR(msg)
        0

let run argv =
    match argv with
    | [||] ->
        printfn "juri repl (juri version 0.1.0)"
        startRepl() |> ignore; 1
    | [|path|] ->
        if File.Exists(path) then
            printfn $"executing file: \"{path}\""
            //printfn "read script from file %A" (fileContent.ToCharArray())
            runScript(File.ReadAllText(path))
        else
            printfn $"file not found: \"{path}\""; 0
    | _ -> printfn "too many arguments"; 0