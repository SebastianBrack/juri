module ParserCombinators


open System


type Position = int


type ParserResult<'ResultType, 'ParserContext> =
    | Succsess of 'ResultType * 'ParserContext * Position 
    | Failure  of string * (Position * Position) option * 'ParserContext

let isSuccsess = function | Succsess _ -> true  | Failure _ -> false
let isFailure  = function | Succsess _ -> false | Failure _ -> true


type Parser<'T, 'ParserContext> = Parser of (CharStream<'ParserContext> -> ParserResult<'T, 'ParserContext>)

and CharStream<'ParserContext>(charseq: char seq, context: 'ParserContext) =
    let content = Seq.toArray charseq 
    let mutable position = 0
    let mutable parserContext = context
    let unpackParser (Parser p) = p

    member this.RunParser(parser: Parser<'T, 'ParserContext>) =
        let (Parser p) = parser
        p this

    member this.GetContext() = parserContext

    member this.SetContext(c) = parserContext <- c

    member this.SetPosition(i) = position <- i
    member this.IncrementPosition(i) = position <- position + 1
    member this.DecrementPosition(i) = position <- position - 1

    member this.HasNext =
        position < content.Length

    member this.HasNextN(n) =
        position + n - 1 >= content.Length

    member this.Next() = content.[position]

    member this.NextN(n) = content.[position .. position + n - 1]

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


let run (Parser p) stream = p stream
let runOn stream (Parser p) = p stream


// ---------------------------------
// combinators and mapping functions
//----------------------------------


/// Maps the result of a Parser according to the mapper to another Parser.
let map mapper parser =
    fun stream ->
        match run parser stream with
        | Succsess (r,c,p) -> Succsess (mapper r, c, p)
        | fail             -> fail
    |> Parser

/// Infix version of map.
let ( |>> ) parser mapper = map mapper parser


/// Applies the parser and changes the Error message to msg if the parser should fail.
let deferr msg parser =
    fun stream ->
        match run parser stream with
        | Failure (_,b,c) -> Failure (msg,b,c)
        | result          -> result
    |> Parser


/// Chains two parsers together and combines the results into a tupel.
let ( .>>. ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        match run left stream with
        | Failure (m,pp,c)    -> Failure (m,pp,c)
        | Succsess (r1,c1,p1) ->
            stream.SetContext(c1)
            stream.IncrementPosition(p1)
            match run right stream with
            | Failure (m,pp,c2) ->
                stream.SetContext(cOriginal)
                stream.DecrementPosition(p1)
                Failure (m,pp,c2)
            | Succsess (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.IncrementPosition(p2)
                Succsess ((r1,r2), c2, p1+p2)
    |> Parser


/// Chains two parsers together. The result of the second parser is kept
/// while the result of the first parser gets omitted.
let ( >>. ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        match run left stream with
        | Failure (a,b,c) as f1 -> f1
        | Succsess (_,c1,p1) ->
            stream.SetContext(c1)
            stream.IncrementPosition(p1)
            match run right stream with
            | Failure (m,pp,c2) ->
                stream.SetContext(cOriginal)
                stream.DecrementPosition(p1)
                Failure (m,pp,c2)
            | Succsess (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.IncrementPosition(p2)
                Succsess (r2, c2, p1+p2)
    |> Parser


/// Chains two parsers together. The result of the first parser is kept
/// while the result of the second parser gets omitted.
let ( .>> ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        match run left stream with
        | Failure (a,b,c) as f1 -> f1
        | Succsess (r1,c1,p1)   ->
            stream.SetContext(c1)
            stream.IncrementPosition(p1)
            match run right stream with
            | Failure (m,pp,c2) ->
                stream.SetContext(cOriginal)
                stream.DecrementPosition(p1)
                Failure (m,pp,c2)
            | Succsess (_,c2,p2) ->
                stream.SetContext(c2)
                stream.IncrementPosition(p2)
                Succsess (r1, c2, p1+p2)
    |> Parser


/// Combines two parsers into a new parser that first tries to apply the
/// first parser and if it fails applies the second parser.
let either left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        match run left stream with
        | Succsess (r1,c1,p1) as s1 ->
            stream.SetContext(c1)
            stream.IncrementPosition(p1)
            s1
        | Failure (m1,pp1,c1) ->
            match run right stream with
            | Failure (m2,pp2,c2) ->
                Failure (m1,pp1,c1)
            | Succsess (r2,c2,p2) as s2 ->
                stream.SetContext(c2)
                stream.IncrementPosition(p2)
                s2
    |> Parser

/// Infix version of either.
let ( <|> ) left right = either left right


/// Works like either but instad of two parsers choice combines a whole sequence of parsers.
//let choice parsers = Seq.reduce either parsers
let coice parsers =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let succseedingParser p =
            match run p stream with
            | Succsess (_) as s -> Some s
            | Failure (_)       -> None
        match Seq.tryPick succseedingParser parsers with
        | None -> Failure ("Nothing parsable was found in the input stream.", None, cOriginal)
        | Some (Succsess (r,c,p)) ->
            stream.SetContext(c)
            stream.IncrementPosition(p)
            Succsess (r,c,p)
        | _ -> Failure ("Nothing parsable was found in the input stream.", None, cOriginal)


/// Chains the parser after itself until it fails. The results get combined
/// into a list. Returns an empty list as result if the first parsing attempt fails.
let many parser =
    let rec innerFnc results stream =
        match run parser stream with
        | Failure (m,pp,c) ->
            Succsess (results, stream.GetContext(), 0) 
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.IncrementPosition(p)
            innerFnc (results @ [r]) stream
    Parser (innerFnc [])


/// Like many but requires at least one successful parse to succeed.
let many1 parser =
    let rec innerFnc (results: 'a list) stream =
        match run parser stream with
        | Failure (m,pp,c) when results.IsEmpty ->
            Failure (m,pp,c)
        | Failure (m,pp,c) ->
            Succsess (results, stream.GetContext(), 0) 
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.IncrementPosition(p)
            innerFnc (results @ [r]) stream
    Parser (innerFnc [])


/// Chains the parser after itself until the boundary parser succseeds. The results get combined
/// into a list. If the end of the stream is reached but the boundary parser didnt succseeds
/// jet the whole parser fails.
(* let until parser boundary =
    let rec innerFnc parser boundary (results: 'a list) (stream: CharStream<'c>) =
        let cOriginal = stream.GetContext()
        match run parser stream with
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition

    Parser (innerFnc parser boundary) *)


/// Applies the parser. If it succeeds it wrappes the result in a list.
/// If it fails it returns an empty list as result.
let optional parser =
    fun stream ->
        match run parser stream with
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.IncrementPosition(p)
            Succsess ([r],c,p)
        | Failure (m,pp,c) ->
            Succsess ([], c, 0)
    |> Parser


// helper function that enables circular references
let createParserForwarder () =
    let implementationDummy : Parser<_,_> ref =
        ref (
            (fun (stream: CharStream<'c>) ->
                Failure ("Dummy was not overwritten with actual implementation", None, stream.GetContext())
            )
            |> Parser
            )
    let forwarder : Parser<_,_> =
        (fun stream -> run !implementationDummy stream) |> Parser
    (forwarder, implementationDummy)


//--------------
// Basic Parsers
//--------------

let pchar c =
    fun (stream: CharStream<_>) ->
        if not stream.HasNext then
            let message = sprintf "Expected: %c but the input stream is empty." c
            Failure (message, None, stream.GetContext())
        else 
            let nextc = stream.Next()
            if nextc <> c then
                let message = sprintf "Expected: %c but instead found %c." c nextc
                Failure (message, None, stream.GetContext())
            else
                Succsess (c, stream.GetContext(), 1)
    |> Parser


let pstring (str: string) =
    fun (stream: CharStream<_>) ->
        if not <| stream.HasNextN(str.Length) then
            let message = sprintf "Expected: %s but the input stream ends." str
            Failure (message, None, stream.GetContext())
        else 
            let nextstr = stream.NextN(str.Length) |> String.Concat
            if nextstr <> str then
                let message = sprintf "Expected: \"%s\" but instead found \"%s\"." str nextstr
                Failure (message, None, stream.GetContext())
            else
                Succsess (str, stream.GetContext(), str.Length)
    |> Parser


let satisfies f =
    fun (stream: CharStream<'c>) ->
        match stream.GetContext() |> f with
        | true  -> Succsess ((), stream.GetContext(), 0)
        | false -> Failure ("", None, stream.GetContext())