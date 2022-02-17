module Internal.CLI

open Internal.Repl
open Internal.Parser
open Internal.ParserCombinators
open System.IO
open Internal.Interpreter
open Internal.CoreLib
open Internal.Runtime

let private runScript script =
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
    let prog = parseProgram (script + "\n")
    match parseProgram (script+"\n") with
    | Success (r,_,_) -> 
        compute r initialState |> ignore
        0
    | _ -> (); 0

let run argv =
    let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
    match argv with
    | [||] ->
        printfn "juri repl (juri version 0.1.0)"
        startRepl() |> ignore; 1
    | [|path|] ->
        if File.Exists(path) then
            printfn $"executing file: \"{path}\""
            let fileContent = File.ReadAllText(path)
            //printfn "read script from file %A" (fileContent.ToCharArray())
            runScript(File.ReadAllText(path))
        else
            printfn $"file not found: \"{path}\""; 0
    | _ -> printfn "too many arguments"; 0