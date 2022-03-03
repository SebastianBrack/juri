namespace Juri.Api 

open Juri.Internal
open Juri.Internal.LanguageModel
open Juri.Internal.Output
open Juri.Internal.Parser
open Juri.Internal.Runtime
open Juri.Internal.CoreLib
open Juri.Internal.Interpreter


type public Interpreter() =
    let mutable program : Instruction list = []
    let mutable parsingOK = true
    let mutable outputStreams = InterpreterOutput()
    member this.ParseJuriProgram(code: string) =
        let parsingResult = parseProgram (code + "\n")
        match parsingResult with
        | ParserCombinators.Success(instructions, _, _) ->
            program <- instructions
            parsingOK <- true
        | _ -> parsingOK <- false
       
    member this.ExecuteProgram() =
        let initialState : ComputationState = (None, createEnvWithCoreLibFunctions(), outputStreams)
        match compute program initialState with
        | Ok _      -> ()
        | Error msg -> outputStreams.Error.Write(msg)