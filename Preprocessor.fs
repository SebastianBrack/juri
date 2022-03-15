module Juri.Internal.Preprocessor
open ParserCombinators
open System
open Parser
open Runtime
open Interpreter
open OutputWriter
open System.IO
open CoreLib

let private ws =
    many (pchar ' ' <|> pchar '\t')

let private import=
    pstring "import" .>> ws
    .>>. (AsUntilB (anyChar()) newlineEOS |> failAsFatal) 
    .>> newline
    |>> fun (_,cs) -> String.Concat(cs)


let rec processImports
        (script : string)
        (outputWriter: IOutputWriter) : InterpreterResult<ComputationState * CharStream<JuriContext>> = 

    let initialState = { ComputationState.Default with Environment = createEnvWithCoreLibFunctions () } 
    let scream = CharStream(script, JuriContext.Default)

    let parserResult = scream.RunParser(import)

    match parserResult with
    | Fatal _ -> Error "Syntax Fehler beim Import Statement."
    | Failure _ -> Ok (initialState, scream)
    | Success (path,_,_) -> 
        if not (File.Exists(path)) then
            Error $"Die Zu importierende Datei \"{path}\" wurde nicht gefunden."
        else
            let importedScript = File.ReadAllText(path)
            let recursivelyPreprocessedScript = processImports importedScript outputWriter
            match recursivelyPreprocessedScript with
            | Error msg -> Error $"Syntax Fehler im Importierten Script: {msg}"
            | Ok (newState, newStream) ->
                let programPResult = newStream.RunParser(juriProgram)
                match programPResult with
                | Failure (msg, _) -> Error $"Syntax Fehler im Importierten Script: {msg}"
                | Fatal (msg, _)   -> Error $"Syntax Fehler im Importierten Script: {msg}"
                | Success (prog, _, _) ->
                    let computedState = compute prog outputWriter newState
                    match computedState with
                    | Error msg -> Error $"Fehler beim Ausführen der Importierten Datei: {msg}"
                    | Ok newState -> Ok (newState, scream)
