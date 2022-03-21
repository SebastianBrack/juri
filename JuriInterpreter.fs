namespace Juri.Api 

open Juri.Internal
open Juri.Internal.LanguageModel
open Juri.Internal.Output
open Juri.Internal.OutputWriter
open Juri.Internal.Parser
open Juri.Internal.ParserCombinators
open Juri.Internal.Runtime
open Juri.Internal.CoreLib
open Juri.Internal.Interpreter


type public Interpreter() =
    let mutable program : JuriProgram = []
    let mutable parsingOK = false
    let mutable outputStreams = InterpreterOutputStreams()
    let outputWriter : IOutputWriter = StreamWriter(outputStreams)
    member this.GetOutputStreams() = outputStreams
    member this.ParsingOk() = parsingOK
    member this.ParseJuriProgram(code: char seq) =
        let charStream = CharStream(Seq.append "\n" code, JuriContext.Default)
        let parsingResult = charStream.RunParser(juriProgram)
        match parsingResult with
        | Success(instructions, _, _) ->
            program <- instructions
            parsingOK <- true
        | Failure(msg, _) ->
            parsingOK <- false
            outputWriter.WriteERR(msg, charStream.GetContext().Line)
        | Fatal(msg, _) ->
            parsingOK <- false
            outputWriter.WriteERR(msg, charStream.GetContext().Line)
    member this.ExecuteProgram() =
        let initialState = { ComputationState.Default with Environment = createEnvWithCoreLibFunctions () }
        compute program outputWriter initialState