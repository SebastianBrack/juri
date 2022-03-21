module Juri.Internal.CLI

open Juri.Internal.OutputWriter
open Repl
open Parser
open ParserCombinators
open System.IO
open Interpreter
open CoreLib
open Runtime
open Preprocessor

            

let private runScript (script: string) =
  
    
    let outputWriter : IOutputWriter = ConsoleWriter()
    
    match processImports (script+"\n") outputWriter with
        | Error _ -> 0
        | Ok (state,steam) -> 
            match steam.RunParser(juriProgram)  with
            | Success (r,_,_) -> 
                compute r outputWriter state
                |> ignore
                0
            | Failure _ -> 0
            | Fatal _ -> 0

let run argv =
    match argv with
    | [||] ->
        printfn "juri repl (juri version 0.1.0)"
        startRepl()
        |> ignore; 1
    | [|path|] ->
        if File.Exists(path) then
            printfn $"executing file: \"{path}\""
            //printfn "read script from file %A" (fileContent.ToCharArray())
            runScript(File.ReadAllText(path))
        else
            printfn $"file not found: \"{path}\""; 0
    | _ -> printfn "too many arguments"; 0