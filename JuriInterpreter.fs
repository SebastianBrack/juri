namespace API

open Internal
open Internal.LanguageModel
open Internal.Parser
open Internal.Runtime
open Internal.CoreLib
open Internal.Interpreter


type public Interpreter() =
    let mutable program : Instruction list = []
    let mutable parsingOK = true
    member this.ParseJuriProgram(code: string) =
        let parsingResult = parseProgram (code + "\n")
        match parsingResult with
        | ParserCombinators.Success(instructions, _, _) ->
            program <- instructions
            parsingOK <- true
        | _ -> parsingOK <- false
       
    member this.ExecuteProgram() =
        let initialState : ComputationState = (None, createEnvWithCoreLibFunctions())
        compute program initialState |> ignore