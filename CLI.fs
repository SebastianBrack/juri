module Juri.Internal.CLI

open Juri.Internal.Output
open Repl
open Parser
open ParserCombinators
open System.IO
open Interpreter
open CoreLib
open Runtime

let private runScript script =
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions(), InterpreterOutput())
    match parseProgram (script+"\n") with
    | Success (r,_,_) -> 
        compute r initialState |> ignore
        0
    | _ -> (); 0

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