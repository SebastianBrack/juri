module ParserCombinators



type Position = int


type CharStream<'ParserContext>(charseq: char seq, context: 'ParserContext) =
    let content = Seq.toArray charseq 
    let mutable position = 0
    let mutable parserContext = context

    member this.runParser(p: Parser<'T, 'ParserContext>) =
        

    member this.GetContext = parserContext

    member this.HasNext =
        position < content.Length

    member this.HasNextN(n) =
        position + n - 1 >= content.Length

    member this.Next() =
        if position >= content.Length then
            None
        else 
            let nextChar = content.[position]
            position <- position + 1
            Some nextChar

    member this.NextN(n) =
        if position + n - 1 >= content.Length then
            None
        else
            let nextString = content.[position .. position + n - 1]
            position <- position + n
            Some nextString

    member this.Peek() =
        if position >= content.Length then
            None
        else
            Some content.[position]

    member this.PeekN(n) =
        if position + n - 1 >= content.Length then
            None
        else
            Some content.[position .. position + n - 1]


type ParserResult<'ResultType, 'ParserContext> =
    | Succsess of 'ResultType * 'ParserContext * Position 
    | Failure  of string * (Position * Position) option * 'ParserContext
    | Fatal    of string * (Position * Position) option


type Parser<'T, 'ParserContext> = Parser of (CharStream -> ParserResult<'T, 'ParserContext>)


let run (Parser p) input = p input

let test (stream: CharStream) = stream.

// ---------------------------------
// combinators and mapping functions
//----------------------------------


/// Maps the result of a Parser according to the mapper to another Parser.
let map mapper parser =
    fun stream ->
        match run parser stream with
        | Ok t = r; Stream = rest} -> Ok { Result = mapper r; Stream = rest }
        | Error perror                   -> Error perror
    |> Parser

/// Infix version of map.
let ( |>> ) parser mapper = map mapper parser


/// Applies the parser and changes the Error message to msg if the parser should fail.
let deferr msg parser =
    fun stream ->
        match run parser stream with
        | Ok presult   -> Ok presult
        | Error perror -> Error { Message = msg; Stream = perror.Stream}
    |> Parser


/// Chains two parsers together and combines the results into a tupel.
let ( .>>. ) left right =
    fun stream ->
        match run left stream with
        | Error perror                     -> Error perror
        | Ok {Result = r1; Stream = rest1} ->
            match run right rest1 with
            | Error perror                     -> Error perror
            | Ok {Result = r2; Stream = rest2} -> Ok { Result = (r1,r2); Stream = rest2 }
    |> Parser


/// Chains two parsers together. The result of the second parser is kept
/// while the result of the first parser gets omitted.
let ( >>. ) left right =
    fun stream ->
        match run left stream with
        | Error perror                    -> Error perror
        | Ok {Result = _; Stream = rest1} ->
            match run right rest1 with
            | Error perror                     -> Error perror
            | Ok {Result = r2; Stream = rest2} -> Ok { Result = r2; Stream = rest2 }
    |> Parser


/// Chains two parsers together. The result of the first parser is kept
/// while the result of the second parser gets omitted.
let ( .>> ) left right =
    fun stream ->
        match run left stream with
        | Error perror                     -> Error perror
        | Ok {Result = r1; Stream = rest1} ->
            match run right rest1 with
            | Error perror                     -> Error perror
            | Ok {Result = _; Stream = rest2} -> Ok { Result = r1; Stream = rest2 }
    |> Parser


/// Combines two parsers into a new parser that first tries to apply the
/// first parser and if it fails applies the second parser.
let either left right =
    fun stream ->
        match run left stream with
        | Ok presult -> Ok presult
        | Error _    ->
            match run right stream with
            | Ok presult   -> Ok presult
            | Error perror -> Error perror
    |> Parser

/// Infix version of either.
let ( <|> ) left right = either left right


/// Works like either but instad of two parsers choice combines a whole sequence of parsers.
let choice parsers = Seq.reduce (<|>) parsers


/// Chains the parser after itself until it fails. The results get combined
/// into a list. Returns an empty list as result if the first parsing attempt fails.
let many parser =
    let rec innerFnc results stream  =
        match run parser stream with
        | Error _                        -> Ok { Result = results; Stream = stream }
        | Ok {Result = r; Stream = rest} -> innerFnc (results @ [r]) rest
    Parser (innerFnc [])


/// Like many but requires at least one successful parse to succeed.
let many1 parser =
    let rec innerFnc results stream  =
        match run parser stream with
        | Error perror when List.isEmpty results -> Error perror
        | Error _                                -> Ok { Result = results; Stream = stream }
        | Ok {Result = r; Stream = rest}         -> innerFnc (results @ [r]) rest
    Parser (innerFnc [])


/// Chains the parser after itself until the boundary parser succseeds. The results get combined
/// into a list. If the end of the stream is reached but the boundary parser didnt succseeds
/// jet the whole parser fails.
let until parser boundary =
    let innerFnc parser boundary originalStream =
        let rec take results stream =
            match run boundary stream with
            | Ok {Result = _; Stream = rest} -> Ok { Result = results; Stream = rest }
            | Error perror ->
                if perror.Stream.Read = None then
                    Error {Message = "The input Stream ended unexpectedly."; Stream = originalStream}
                else 
                    match run parser stream with
                    | Error _                        -> Error { Message = "The boundary was not reached before the parser failed"; Stream = originalStream}
                    | Ok {Result = r; Stream = rest} -> take (results @ [r]) rest
        take [] originalStream
    Parser (innerFnc parser boundary)


/// Applies the parser. If it succeeds it wrappes the result in a list.
/// If it fails it returns an empty list as result.
let optional parser =
    fun stream ->
        match run parser stream with
        | Ok {Result = r; Stream = rest} -> Ok { Result = [r]; Stream = rest }
        | Error _                        -> Ok { Result = []; Stream = stream }
    |> Parser


// helper function that enables circular references
let createParserForwarder () =
    let implementationDummy : Parser<_> ref =
        ref ((fun stream -> Error { Message = "Dummy was not overwritten with actual implementation"; Stream = stream }) |> Parser)
    let forwarder : Parser<_> =
        (fun stream -> run !implementationDummy stream) |> Parser
    (forwarder, implementationDummy)


//--------------
// Basic Parsers
//--------------

let pchar c =
    fun (stream : CharStream) ->
        match stream.Read with
        | None -> Error { Message = sprintf "Expected: %c but the input stream is empty." c; Stream = stream }
        | Some (x, rest) when x = c -> Ok { Result = x; Stream = rest}
        | Some (x, _) -> Error { Message = sprintf "Expected: %c but instead found %c." c x; Stream = stream}
    |> Parser


let anybut chars =
    fun (stream : CharStream) ->
        match stream.Read with
        | None -> Error { Message = sprintf "Expected something that is not %A but the input stream is empty." chars; Stream = stream }
        | Some (x, _) when Seq.contains x chars -> Error { Message = sprintf "Expected something different then %c." x; Stream = stream }
        | Some (x, rest) -> Ok { Result = x; Stream = rest}
    |> Parser


let rec pstring str =
    str
    |> Seq.map (fun c -> pchar c |>> string)
    |> Seq.reduce (fun l r -> l .>>. r |>> (fun (a,b) -> a + b))