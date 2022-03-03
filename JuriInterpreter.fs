﻿namespace Juri.Api 

open Juri.Internal
open Juri.Internal.LanguageModel
open Juri.Internal.Output
open Juri.Internal.Parser
open Juri.Internal.Runtime
open Juri.Internal.CoreLib
open Juri.Internal.Interpreter


type public Interpreter() =
    let mutable program : Instruction list = []
    let mutable parsingOK = false
    let mutable outputStreams = InterpreterOutput()
    member this.GetOutputStreams() = outputStreams
    member this.ParsingOk() = parsingOK
    member this.ParseJuriProgram(code: string) =
        let parsingResult = parseProgram (code + "\n")
        match parsingResult with
        | ParserCombinators.Success(instructions, _, _) ->
            program <- instructions
            parsingOK <- true
        | ParserCombinators.Failure(msg, _) ->
            parsingOK <- false
            outputStreams.Error.Write(msg)
        | ParserCombinators.Fatal(msg, _) ->
            parsingOK <- false
            outputStreams.Error.Write(msg)
    member this.ExecuteProgram() =
        let initialState : ComputationState = (None, createEnvWithCoreLibFunctions(), outputStreams)
        match compute program initialState with
        | Ok _      -> ()
        | Error msg -> outputStreams.Error.Write(msg)