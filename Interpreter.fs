module Juri.Internal.Interpreter

open System
open Juri.Internal.OutputWriter
open LanguageModel
open Runtime



let rec private computeLoop
        (con: Expression)
        (rep: bool)
        (body: Instruction list)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state
    match eval outputWriter state con with
    | Error e       -> Error e
    | Ok 0.         -> Ok state
    | Ok _ when rep ->
            match (compute body outputWriter state) with
            | Error e -> Error e
            | Ok x -> computeLoop con rep body outputWriter x
    | Ok _          -> (compute body outputWriter state)


and private computeAssignment
        (id, exp)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state
    let addVariableToState x =
        let newEnv = env |> Map.add id (Variable x)
        Ok (Some x, newEnv)
    match (env.TryFind id) with
    | Some (CustomFunction _) | Some (ProvidedFunction _) -> Error (sprintf "%A ist eine Funktion und kann keinen Wert zugewiesen bekommen." id)
    | _ -> eval outputWriter state exp >>= addVariableToState


and private computeFunctionDefinition
        (id, argNames, body)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state
    let addFunctionToState () =
        let newEnv = env |> Map.add id (CustomFunction (argNames, body))
        Ok (lastExp, newEnv)
    match (env.TryFind id) with
    | Some _ -> Error (sprintf "Der Name %A wird bereits verwendet und kann nicht neu definiert werden." id)
    | None -> addFunctionToState ()


and compute
        (prog: Instruction list)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state
    match prog with
    | [] -> Ok state
    | instruction :: tail ->
        match instruction with
        | Expression exp ->
            match eval outputWriter state exp with
            | Ok x -> compute tail outputWriter (Some x, env)
            | Error e -> Error e
        | Assignment (id,exp) ->
            computeAssignment (id, exp) outputWriter state
            >>= compute tail outputWriter
        | FunctionDefinition (id, argNames, body) ->
            computeFunctionDefinition (id, argNames, body) state
            >>= compute tail outputWriter
        | Loop (con, rep, body) ->
            computeLoop con rep body outputWriter state
            >>= compute tail outputWriter
        | OperatorDefinition (BinaryOperator opName, leftName, rightName, body) ->
            computeFunctionDefinition (Identifier opName, [leftName; rightName], body) state
            >>= compute tail outputWriter


and private eval
        (outputWriter: IOutputWriter)
        (state: ComputationState)
        (exp: Expression) : InterpreterResult<float> =
    
    let lastExp, env = state
    match exp with
    | LiteralNumber x -> Ok x
    | VariableReference id ->
        match (Map.tryFind id env) with
        | None -> Error (sprintf "Der Verweis auf %A konnt nicht aufgelöst werden." id)
        | Some (CustomFunction _) -> Error (sprintf "%A ist keine Variable sondern eine Funktion" id)
        | Some (ProvidedFunction _) -> Error (sprintf "%A ist keine Variable sondern eine Funktion." id)
        | Some (Variable x) -> Ok x
    | FunctionCall (id, args) ->
        match (Map.tryFind id env) with
        | None -> Error (sprintf "Der Verweis auf %A konnt nicht aufgelöst werden." id)
        | Some (Variable _) -> Error (sprintf "%A is keine Funktion sondern eine Variable." id)
        | Some (ProvidedFunction f) -> evalList args outputWriter state >>= (f outputWriter)
        | Some (CustomFunction (argNames, body)) -> evalList args outputWriter state >>= evalCustomFunction (argNames, body) outputWriter state
    | Binary ((BinaryOperator op), left, right) ->
        match (Map.tryFind (Identifier op) env) with
        | None -> Error (sprintf "Der Operator %A ist nicht definiert" op)
        | Some (Variable _) -> Error (sprintf "%A is kein Operator sondern eine Variable." id)
        | Some (ProvidedFunction f) -> evalList [left;right] outputWriter state >>= (f outputWriter)
        | Some (CustomFunction (argNames, body)) -> evalList [left;right] outputWriter state >>= (evalCustomFunction (argNames, body) outputWriter state)


and private evalList
        (expList: Expression list)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<float list> =
    
    let lastExp, env = state
    let appender results elem =
        match results, elem with
        | Error _, _ -> results
        | Ok _, Error e -> Error e
        | Ok xs, Ok x -> Ok (xs @ [x])
    expList
    |> List.map (eval outputWriter state)
    |> List.fold appender (Ok [])


and private evalCustomFunction
        (argNames, body)
        (outputWriter: IOutputWriter)
        (state: ComputationState)
        (args: float list) : InterpreterResult<float> =
    let lastExp, env = state
    if argNames.Length <> args.Length then
        Error (sprintf "Diese Funktion erwarte %i Argumente - es wurden aber %i übergeben." argNames.Length args.Length)
    else
        let functionFilter _ = function | CustomFunction _ | ProvidedFunction _ -> true | _ -> false
        let scopedVariables = args |> List.map Variable |> List.zip argNames
        let scopedFunctions = env |> Map.filter functionFilter |> Map.toList
        let functionEnv = Map (scopedVariables @ scopedFunctions)
        let returnValue = compute body outputWriter (None, functionEnv)
        match returnValue with
        | Error e -> Error e
        | Ok (None, _) -> Error "Die Funktion hat keinen Wert zurückgegeben"
        | Ok (Some x, _) -> Ok x
