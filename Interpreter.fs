module Juri.Internal.Interpreter

open System
open Juri.Internal.OutputWriter
open LanguageModel
open Runtime



// helper functions


let rec private computeLoop
        (con: Expression)
        (rep: bool)
        (body: JuriProgram)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    match eval outputWriter state con with
    | Error e       -> Error e
    | Ok 0.         -> Ok state
    | Ok _ when rep ->
            match (compute body outputWriter state) with
            | Error e -> Error e
            | Ok nextState when nextState.ReturnFlag = true -> Ok nextState
            | Ok nextState when nextState.BreakFlag = true -> Ok {nextState with BreakFlag = false}
            | Ok nextState -> computeLoop con rep body outputWriter nextState
    | Ok _ -> (compute body outputWriter state)


and private computeAssignment
        (id, exp)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let env = state.Environment
    let addVariableToState x =
        let newEnv = env |> Map.add id (Variable x)
        Ok {state with LastExpression = None; Environment = newEnv}
    match (env.TryFind id) with
    | None | Some (Variable _) -> eval outputWriter state exp >>= addVariableToState
    | _ -> Error $"{id} ist keine Variable und kann keinen Wert zugewiesen bekommen."
    
    
and private computeListAssignment
        (id, expressions)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
        
    let env = state.Environment
    let addListToState (xs: float list) =
        let newEnv = env |> Map.add id (List (List.toArray xs))
        Ok {state with LastExpression = None; Environment = newEnv}
    match (env.TryFind id) with
    | None | Some (List _) ->
        evalList expressions outputWriter state
        >>= addListToState
    | _ -> Error $"{id} ist keine Liste und kann keine entsprechenden Werte zugewiesen bekommen."
    
    
    
and private computeListAssignmentWithRange
        (id, lowerBoundExpression, upperBoundExpression)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
        
    let env = state.Environment
    match (env.TryFind id) with
    | None | Some (List _) ->
        let lowerEvalResult = eval outputWriter state lowerBoundExpression
        let upperEvalResult = eval outputWriter state upperBoundExpression
        match (lowerEvalResult, upperEvalResult) with
        | Ok low, Ok up ->
            let newEnv = env |> Map.add id (List [|low..up|])
            Ok {state with LastExpression = None; Environment = newEnv}
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
    | _ -> Error $"{id} ist keine Liste und kann keine entsprechenden Werte zugewiesen bekommen."
    
    
    
and private computeListInitialisationWithValue
        (listName, sizeExpression, valueExpression)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let env = state.Environment
    match (env.TryFind listName) with
    | None | Some (List _) ->
        let sizeEvalResult = eval outputWriter state sizeExpression
        let valueEvalResult = eval outputWriter state valueExpression
        match (sizeEvalResult, valueEvalResult) with
        | Ok size, Ok value ->
            let newList = Array.create (int size) value
            let newEnv = env |> Map.add listName (List newList)
            Ok {state with LastExpression = None; Environment = newEnv}
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
    | _ -> Error $"{id} ist keine Liste und kann keine entsprechenden Werte zugewiesen bekommen."
    
            
            
and private computeListInitialisationWithCode
        (listName, sizeExpression, indexName, generatorCode) 
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let env = state.Environment
    let generateListElement i state =
        let generatorResult =
            computeAssignment (indexName, LiteralNumber i) outputWriter state
            >>= compute generatorCode outputWriter
        match generatorResult with
        | Error msg -> Error $"Fehler beim Generieren des Listenelements: {msg}"
        | Ok {LastExpression = None} -> Error $"Fehler beim Generieren des Listenelements: Der Generatorcode hat keinen Wert zurückgegeben."
        | Ok {LastExpression = Some x} -> Ok x
    match (env.TryFind listName) with
    | None | Some (List _) ->
        let sizeEvalResult = eval outputWriter state sizeExpression
        match sizeEvalResult with
        | Ok size ->
            if size < 0. then
                Error $"Es kann keine List der Länge {size} erstellt werden."
            else
                let newList : float array = Array.create (int size) 0.
                let mutable i = 0
                let mutable cancelFlag = false
                let mutable generationError = Error ""
                while i < (int size) && not cancelFlag do
                    let element = generateListElement i state
                    match element with
                    | Error msg ->
                        cancelFlag <- true
                        generationError <- Error msg
                    | Ok x ->
                        newList[i] <- x
                    i <- i+1
                if cancelFlag then
                    generationError
                else
                    let newEnv = env |> Map.add listName (List newList)
                    Ok {
                        LastExpression = None
                        Environment = newEnv
                        BreakFlag = false
                        ReturnFlag = false }
        | Error msg -> Error msg
    | _ -> Error $"{id} ist keine Liste und kann keine entsprechenden Werte zugewiesen bekommen."
    
and private computeListElementAssignment
        (id, indexExpression, valueExpression)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    let env = state.Environment
    match (env.TryFind id) with
    | Some (List xs) ->
        let indexEvalResult = eval outputWriter state indexExpression
        let valueEvalResult = eval outputWriter state valueExpression
        match (indexEvalResult, valueEvalResult ) with
        | Ok index, Ok value ->
            let trueIndex = if index < 0.0 then xs.Length + int index else int index
            if trueIndex >= 0 && trueIndex < xs.Length then
                xs[trueIndex] <- value
            Ok {state with LastExpression = Some value; Environment = env}
        | Error msg, _ -> Error msg
        | _, Error msg -> Error msg
    | _ -> Error $"{id} ist keine Liste."
    

and private computeFunctionDefinition
        (id, argNames, body)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let lastExp, env = state.LastExpression, state.Environment
    let addFunctionToState () =
        let newEnv = env |> Map.add id (CustomFunction (argNames, body))
        Ok {state with LastExpression = lastExp; Environment = newEnv}
    match (env.TryFind id) with
    | Some _ -> Error $"Der Name {id} wird bereits verwendet und kann nicht neu definiert werden."
    | None -> addFunctionToState ()


and private computeListIteration
        (listName, valueName, loopBody)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    let env = state.Environment
    let computeIteration state value =
        computeAssignment (valueName, LiteralNumber value) outputWriter state
        >>= compute loopBody outputWriter
    let rec iterate (xs: float array) pos state =
        if pos = xs.Length then
            Ok state
        else
            computeIteration state xs[pos]
            |> Runtime.map (fun state -> { state with BreakFlag = false; ReturnFlag = false })
            >>= iterate xs (pos+1) 
    match (env.TryFind listName) with
    | Some (List xs) -> iterate xs 0 state
    | _ -> Error $"{listName} ist keine Liste."


and compute
        (program: JuriProgram)
        (outputWriter: IOutputWriter)
        (state: ComputationState) : InterpreterResult<ComputationState> =
    
    match program with
    | [] -> Ok state
    | _ when state.BreakFlag || state.ReturnFlag -> Ok state
    | (instruction, line) :: tail ->
        let mutable discardTail = false
        let computationResult =
            match instruction with
            | Break ->
                discardTail <- true
                Ok {state with BreakFlag = true}
            | Return exp ->
                match eval outputWriter state exp with
                | Ok x ->
                    discardTail <- true
                    Ok {state with ReturnFlag = true; LastExpression = Some x}
                | Error e ->
                    Error e
            | Expression exp ->
                match eval outputWriter state exp with
                | Ok x -> Ok {state with LastExpression = Some x}
                | Error e -> Error e
            | Assignment (id,exp) ->
                computeAssignment (id, exp) outputWriter state
            | FunctionDefinition (id, argNames, body) ->
                computeFunctionDefinition (id, argNames, body) state
            | Loop (con, rep, body) ->
                computeLoop con rep body outputWriter state
            | OperatorDefinition (BinaryOperator opName, leftName, rightName, body) ->
                computeFunctionDefinition (Identifier opName, [leftName; rightName], body) state
            | ListAssignment(listName, expressions) ->
                computeListAssignment (listName, expressions) outputWriter state
            | ListAssignmentWithRange (listName, lowerBound, upperBound) ->
                computeListAssignmentWithRange (listName, lowerBound, upperBound) outputWriter state
            | ListInitialisationWithValue (listName, size, value) ->
                computeListInitialisationWithValue (listName, size, value) outputWriter state
            | ListInitialisationWithCode(listName, size, indexName, instructions) ->
                computeListInitialisationWithCode (listName, size, indexName, instructions) outputWriter state
            | ListElementAssignment(identifier, indexExpression, valueExpression) ->
                computeListElementAssignment (identifier, indexExpression, valueExpression) outputWriter state
            | Iteration(listName, elementName, loopBody) ->
                computeListIteration (listName, elementName, loopBody) outputWriter state
        
        match computationResult with
        | Ok newState -> compute tail outputWriter newState
        | Error msg ->
            outputWriter.WriteERR(msg, line)
            Error msg


and private eval
        (outputWriter: IOutputWriter)
        (state: ComputationState)
        (exp: Expression) : InterpreterResult<float> =
    
    let env = state.Environment
    
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
    | ParenthesizedExpression expression ->
        eval outputWriter state expression


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
    let env = state.Environment
    if argNames.Length <> args.Length then
        Error (sprintf "Diese Funktion erwarte %i Argumente - es wurden aber %i übergeben." argNames.Length args.Length)
    else
        let functionFilter _ = function | CustomFunction _ | ProvidedFunction _ -> true | _ -> false
        let scopedVariables = args |> List.map Variable |> List.zip argNames
        let scopedFunctions = env |> Map.filter functionFilter |> Map.toList
        let functionEnv = Map (scopedVariables @ scopedFunctions)
        let functionComputationState = {
            LastExpression = None
            Environment = functionEnv
            BreakFlag = false
            ReturnFlag = false }
        let returnValue = compute body outputWriter functionComputationState
        match returnValue with
        | Error e -> Error e
        | Ok {LastExpression = None} -> Error "Die Funktion hat keinen Wert zurückgegeben"
        | Ok {LastExpression = Some x} -> Ok x
