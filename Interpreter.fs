module Internal.Interpreter

open System
open LanguageModel
open Runtime



let rec private computeLoop (con: Expression) (rep: bool) (body: Instruction list) (state: ComputationState) : EvalResult<ComputationState> =
    let lastExp, env = state
    match eval env con with
    | Error e       -> Error e
    | Ok 0.         -> Ok state
    | Ok _ when rep ->
            match (compute body state) with
            | Error e -> Error e
            | Ok x -> computeLoop con rep body x
    | Ok _          -> (compute body state)


and private computeAssignment (id, exp) (state: ComputationState) : EvalResult<ComputationState> =
    let lastExp, env = state
    let addVariableToState x = Ok (lastExp, env |> Map.add id (Variable x))
    match (env.TryFind id) with
    | Some (CustomFunction _) | Some (ProvidedFunction _) -> Error (sprintf "%A ist eine Funktion und kann keinen Wert zugewiesen bekommen." id)
    | _ -> eval env exp >=> addVariableToState


and private computeFunctionDefinition (id, argNames, body) (state: ComputationState) : EvalResult<ComputationState> =
    let lastExp, env = state
    let addFunctionToState () = Ok (lastExp, env |> Map.add id (CustomFunction (argNames, body)))
    match (env.TryFind id) with
    | Some _ -> Error (sprintf "Der Name %A wird bereits verwendet und kann nicht neu definiert werden." id)
    | None -> addFunctionToState ()


and compute (prog: Instruction list) (state: ComputationState) : EvalResult<ComputationState> =
    let lastExp, env = state
    match prog with
    | [] -> Ok state
    | inst :: tail ->
        match inst with
        | Expression exp ->
            match eval env exp with
            | Ok x -> compute tail (Some x, env)
            | Error e -> Error e
        | Assignment (id,exp) ->
            computeAssignment (id, exp) state
            >>= compute tail
        | FunctionDefinition (id, argNames, body) ->
            computeFunctionDefinition (id, argNames, body) state
            >>= compute tail
        | Loop (con, rep, body) ->
            computeLoop con rep body state
            >>= compute tail
        | OperatorDefinition ((BinaryOperator opName), leftName, rightName, body) ->
            computeFunctionDefinition (Identifier opName, [leftName; rightName], body) state
            >>= compute tail


and private eval (env: Environment) (exp: Expression) : EvalResult<float> =
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
        | Some (ProvidedFunction f) -> evalList args env >=> f
        | Some (CustomFunction (argNames, body)) -> evalList args env >=> evalCustomFunction (argNames, body) env
    | Binary ((BinaryOperator op), left, right) ->
        match (Map.tryFind (Identifier op) env) with
        | None -> Error (sprintf "Der Operator %A ist nicht definiert" op)
        | Some (Variable _) -> Error (sprintf "%A is kein Operator sondern eine Variable." id)
        | Some (ProvidedFunction f) -> evalList [left;right] env >=> f
        | Some (CustomFunction (argNames, body)) -> evalList [left;right] env >=> evalCustomFunction (argNames, body) env


and private evalList (expList : Expression list) (env: Environment) : EvalResult<float list> =
    let appender state elem =
        match state, elem with
        | Error _, _ -> state
        | Ok _, Error e -> Error e
        | Ok xs, Ok x -> Ok (xs @ [x])
    expList
    |> List.map (eval env)
    |> List.fold appender (Ok [])


and private evalCustomFunction (argNames, body) (env: Environment) (args: float list) : EvalResult<float> =
    if argNames.Length <> args.Length then
        Error (sprintf "Diese Funktion erwarte %i Argumente - es wurden aber %i übergeben." argNames.Length args.Length)
    else
        let functionFilter _ = function | CustomFunction _ | ProvidedFunction _ -> true | _ -> false
        let scopedVariables = args |> List.map Variable |> List.zip argNames
        let scopedFunctions = env |> Map.filter functionFilter |> Map.toList
        let functionEnv = Map (scopedVariables @ scopedFunctions)
        let returnValue = compute body (None, functionEnv)
        match returnValue with
        | Error e -> Error e
        | Ok (None, _) -> Error "Die Funktion hat keinen Wert zurückgegeben"
        | Ok (Some x, _) -> Ok x
