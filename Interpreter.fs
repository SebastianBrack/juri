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
    
    let _, env = state
    let addVariableToState x =
        let newEnv = env |> Map.add id (Variable x)
        Ok (Some x, newEnv)
    match (env.TryFind id) with
    | None | Some (Variable _) -> eval outputWriter state exp >>= addVariableToState
    | _ -> Error $"{id} ist keine Variable und kann keinen Wert zugewiesen bekommen."
    
    
and private computeListAssignment
        (id, expressions)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
        
    let _, env = state
    let addListToState (xs: float list) =
        let newEnv = env |> Map.add id (List (List.toArray xs))
        Ok (None, newEnv)
    match (env.TryFind id) with
    | None | Some (List _) ->
        evalList expressions outputWriter state
        >>= addListToState
    | _ -> Error $"{id} ist keine Liste und kann keine entsprechenden Werte zugewiesen bekommen."
    
    
and private computeListElementAssignment
        (id, indexExpression, valueExpression)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    let _, env = state
    match (env.TryFind id) with
    | Some (List xs) ->
        let indexEvalResult = eval outputWriter state indexExpression
        let valueEvalResult = eval outputWriter state valueExpression
        match (indexEvalResult, valueEvalResult ) with
        | (Ok index, Ok value) ->
            let trueIndex = if index < 0.0 then xs.Length + int index else int index
            if trueIndex >= 0 && trueIndex < xs.Length then
                xs[trueIndex] <- value
            Ok (Some value, env)
        | (Error msg, _) -> Error msg
        | (_, Error msg) -> Error msg
    | _ -> Error $"{id} ist keine Liste."
    

and private computeFunctionDefinition
        (id, argNames, body)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state
    let addFunctionToState () =
        let newEnv = env |> Map.add id (CustomFunction (argNames, body))
        Ok (lastExp, newEnv)
    match (env.TryFind id) with
    | Some _ -> Error $"Der Name {id} wird bereits verwendet und kann nicht neu definiert werden."
    | None -> addFunctionToState ()


and private computeListIteration
        (listName, valueName, loopBody)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let _, env = state
    let computeIteration state value =
        computeAssignment (valueName, LiteralNumber value) outputWriter state
        >>= compute loopBody outputWriter
    let rec iterate (xs: float array) pos state =
        if xs.Length >= pos then
            Ok state
        else
            computeIteration state xs[pos]
            >>= iterate xs (pos+1) 
    match (env.TryFind listName) with
    | Some (List xs) -> iterate xs 0 state
    | _ -> Error $"{listName} ist keine Liste."


and compute
        (program: Instruction list)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let _, env = state
    match program with
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
        | ListAssignment(listName, expressions) ->
            computeListAssignment (listName, expressions) outputWriter state
            >>= compute tail outputWriter
        | ListElementAssignment(identifier, indexExpression, valueExpression) ->
            computeListElementAssignment (identifier, indexExpression, valueExpression) outputWriter state
            >>= compute tail outputWriter
        | Iteration(listName, elementName, loopBody) ->
            computeListIteration (listName, elementName, loopBody) outputWriter state
            >>= compute tail outputWriter


and private eval
        (outputWriter: IOutputWriter)
        (state: ComputationState)
        (exp: Expression) : InterpreterResult<float> =
    
    let _, env = state
    match exp with
    | LiteralNumber x -> Ok x
    | VariableReference id ->
        match (Map.tryFind id env) with
        | Some (Variable x) -> Ok x
        | Some _ -> Error $"{id} ist keine Variable"
        | None -> Error $"Der Verweis auf %A{id} konnt nicht aufgelöst werden."
    | ListAccess (id, indexExpression) ->
        match (Map.tryFind id env) with
        | Some (List xs) ->
            eval outputWriter state indexExpression
            >>= (int >> Ok)
            >>= evalListAccess xs
        | Some _ -> Error $"{id} ist keine Liste."
        | None -> Error $"Die Liste {id} existiert nicht."
    | ListLength id ->
        match (Map.tryFind id env) with
        | Some (List xs) -> Ok xs.Length
        | _ -> Error $"Die Liste {id} existiert nicht."
    | FunctionCall (id, args) ->
        match (Map.tryFind id env) with
        | Some (ProvidedFunction f) ->
            evalList args outputWriter state
            >>= (f outputWriter)
        | Some (CustomFunction (argNames, body)) ->
            evalList args outputWriter state
            >>= evalCustomFunction (argNames, body) outputWriter state
        | Some _ -> Error $"{id} ist keine Funktion."
        | None -> Error $"Der Verweis auf %A{id} konnte nicht aufgelöst werden."
    | Binary (BinaryOperator op, left, right) ->
        match (Map.tryFind (Identifier op) env) with
        | Some (ProvidedFunction f) ->
            evalList [left;right] outputWriter state
            >>= (f outputWriter)
        | Some (CustomFunction (argNames, body)) ->
            evalList [left;right] outputWriter state
            >>= (evalCustomFunction (argNames, body) outputWriter state)
        | Some _ -> Error $"{id} ist kein Operator."
        | None -> Error $"Der Operator %A{op} ist nicht definiert"


and private evalListAccess
        (list: float array)
        (index: int) : InterpreterResult<float> =
    
    let trueIndex =
        if index < 0
            then list.Length + index
            else index
    if trueIndex >= 0 && trueIndex < list.Length
        then Ok list[trueIndex]
        else Error $"Der Index {index} liegt ausserhalb des Umfangs der Liste."

and private evalList
        (expList: Expression list)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<float list> =
    
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
    let _, env = state
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
