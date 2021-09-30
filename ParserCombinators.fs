module ParserCombinators


open System


type Position = int

type ParserError = Position 


type ParserResult<'ResultType, 'ParserContext> =
    | Succsess of 'ResultType * 'ParserContext * Position 
    | Failure  of string * ParserError option
    | Fatal    of string * ParserError option

let isSuccsess = function | Succsess _ -> true  | Failure _ -> false | Fatal _ -> false
let isFailure  = function | Succsess _ -> false | Failure _ -> true  | Fatal _ -> true
let isFatal    = function | Succsess _ -> false | Failure _ -> false | Fatal _ -> true


type Parser<'T, 'ParserContext> = Parser of (CharStream<'ParserContext> -> ParserResult<'T, 'ParserContext>)

and CharStream<'ParserContext>(charseq: char seq, context: 'ParserContext) =
    let content = Seq.toArray charseq 
    let mutable parserContext = context
    let mutable position : Position = 0


    member this.RunParser(parser: Parser<'T, 'ParserContext>) =
        let (Parser p) = parser
        p this

    member this.GetContext() = parserContext
    member this.SetContext(c) = parserContext <- c

    member this.GetPosition() = position
    member this.SetPosition(i) =
        //eprintfn "set stream position to %i %A" i (if this.HasNext then [content.[position]] else ['x'])
        position <- i

    member this.GetChars = content

    member this.HasNext =
        position < content.Length

    member this.HasNextN(n) =
        position + n - 1 < content.Length

    member this.Next = content.[position]

    member this.NextN(n) = content.[position .. position + n - 1]



let run (Parser p) stream = p stream
let runOn stream (Parser p) = p stream


// ---------------------------------
// combinators and mapping functions
//----------------------------------


let join2 (a,bs) = a :: bs


/// Maps the result of a Parser according to the mapper to another Parser.
let map mapper parser =
    fun stream ->
        match run parser stream with
        | Succsess (r,c,p) -> Succsess (mapper r, c, p)
        | Failure (m,e)    -> Failure (m,e)
        | Fatal (m,e)      -> Fatal (m,e)
    |> Parser

/// Infix version of map.
let ( |>> ) parser mapper = map mapper parser


/// Applies the parser and changes the Error message to msg if the parser should fail.
let deferr msg parser =
    fun stream ->
        match run parser stream with
        | Failure (_,b)       -> Failure (msg,b)
        | Fatal (mOriginal,b) -> Fatal (mOriginal,b)
        | result              -> result
    |> Parser


let failAsFatal parser =
    fun stream ->
        match run parser stream with
        | Succsess (r,c,p) -> Succsess (r,c,p)
        | Failure (m,e)    -> Fatal (m,e)
        | Fatal (m,e)      -> Fatal (m,e)
    |> Parser


/// <summary>
/// Updates the context in the char stream according to the given function f
/// if the parser succseeds
/// </summary>
/// <param name="f">
/// The context generating function. Takes the result of the succeeding parser and the current context and generates a new context.
/// </param>
/// <param name="parser">
/// The parser that you want to do the context update
/// </param>
let updateContext f parser =
    fun stream ->
        match run parser stream with
        | Failure (m,e) ->
            Failure (m,e)
        | Fatal (m,e) ->
            Fatal (m,e)
        | Succsess (r,c,p) ->
            let newContext = stream.GetContext() |> f r
            //eprintfn "contextUpdate %A" newContext
            stream.SetContext(newContext)
            Succsess (r, newContext, p)
    |> Parser



/// The overall Parser succseeds if the given predicate returns true and fails if it returns false
let satisfies predicate parser =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run parser stream with
        | Failure (m,e) ->
            Failure (m,e)
        | Fatal (m,e) ->
            Fatal (m,e)
        | Succsess (r,c,p) ->
            match stream.GetContext() |> predicate r with
            | false ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure ("", None)
            | true  ->
                Succsess (r,c,p)
    |> Parser


/// Chains two parsers together and combines the results into a tupel.
let ( .>>. ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run left stream with
        | Failure (m,pe)      -> Failure (m,pe)
        | Fatal (m,pe)        -> Fatal (m,pe)
        | Succsess (r1,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Succsess (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Succsess ((r1,r2), c2, p2)
    |> Parser


/// Chains two parsers together. The result of the second parser is kept
/// while the result of the first parser gets omitted.
let ( >>. ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run left stream with
        | Failure (m,e)      -> Failure (m,e)
        | Fatal (m,e)        -> Fatal (m,e)
        | Succsess (_,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Succsess (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Succsess (r2, c2, p2)
    |> Parser


/// Chains two parsers together. The result of the first parser is kept
/// while the result of the second parser gets omitted.
let ( .>> ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run left stream with
        | Failure (m,e)       -> Failure (m,e)
        | Fatal (m,e)         -> Fatal (m,e)
        | Succsess (r1,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Succsess (_,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Succsess (r1, c2, p2)
    |> Parser


/// Combines two parsers into a new parser that first tries to apply the
/// first parser and if it fails applies the second parser.
let either left right =
    fun (stream: CharStream<'c>) ->
        match run left stream with
        | Succsess (r1,c1,p1) as s1 ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            s1
        | Fatal (m,e) ->
            Fatal (m,e)
        | Failure (m1,pe1) ->
            match run right stream with
            | Failure (m2,pe2) ->
                Failure (m1,pe1)
            | Fatal (m,e) ->
                Fatal (m,e)
            | Succsess (r2,c2,p2) as s2 ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                s2
    |> Parser

/// Infix version of either.
let ( <|> ) left right = either left right


/// Works like either but instad of two parsers choice combines a whole sequence of parsers.
//let choice parsers = Seq.reduce either parsers
let choice parsers =
    fun (stream: CharStream<'c>) ->
        let succseedingOrFatalParser p =
            match run p stream with
            | Succsess (_) as s -> Some s
            | Fatal (_) as f    -> Some f
            | Failure (_)       -> None
        match Seq.tryPick succseedingOrFatalParser parsers with
        | None                    -> Failure ("Nothing parsable was found in the input stream.", None)
        | Some (Fatal (m,e))      -> Fatal (m,e)
        | Some (Succsess (r,c,p)) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            Succsess (r,c,p)
        | _ -> Failure ("Nothing parsable was found in the input stream.", None)
    |> Parser


/// Chains the parser after itself until it fails. The results get combined
/// into a list. Returns an empty list as result if the first parsing attempt fails.
let many parser =
    let rec innerFnc results stream =
        match run parser stream with
        | Fatal (m,e) ->
            Fatal (m,e)
        | Failure (_) ->
            Succsess (results, stream.GetContext(), stream.GetPosition()) 
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            innerFnc (results @ [r]) stream
    Parser (innerFnc [])


/// Like many but requires at least one successful parse to succeed.
let many1 parser =
    let rec innerFnc (results: 'a list) stream =
        match run parser stream with
        | Fatal (m,e) ->
            Fatal (m,e)
        | Failure (m,pe) when results.IsEmpty ->
            Failure (m,pe)
        | Failure (m,pe) ->
            Succsess (results, stream.GetContext(), stream.GetPosition()) 
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            innerFnc (results @ [r]) stream
    Parser (innerFnc [])


/// Applies the parser. If it succeeds it wrappes the result in a list.
/// If it fails it returns an empty list as result.
let optional parser =
    fun stream ->
        match run parser stream with
        | Succsess (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            Succsess ([r],c,p)
        | Failure (m,pp) ->
            Succsess ([], stream.GetContext(), stream.GetPosition())
        | Fatal (m,e) ->
            Fatal (m,e)
    |> Parser


// helper function that enables circular references
let createParserForwarder () =
    let implementationDummy : Parser<_,_> ref =
        ref (
            (fun (stream: CharStream<'c>) ->
                Failure ("Dummy was not overwritten with actual implementation", None)
            )
            |> Parser
            )
    let forwarder : Parser<_,_> =
        (fun stream -> run !implementationDummy stream) |> Parser
    (forwarder, implementationDummy)


//--------------
// Basic Parsers
//--------------

let createEOS<'c> () =
    fun (stream: CharStream<_>) ->
        let c: 'c = stream.GetContext()
        if not stream.HasNext then
            Succsess ((), c, stream.GetPosition())
        else
            Failure ("End of stream expected but there is more stuff.", Some (stream.GetPosition()))
    |> Parser



let createNewline<'c> () =
    fun (stream: CharStream<_>) ->
        let c: 'c = stream.GetContext()
        if stream.HasNext then
            let nextc = stream.Next
            if nextc = '\n' || nextc = '\r' || nextc = '\u0085' || nextc = '\u2029' || nextc = '\uffff' then
                Succsess ((), c, stream.GetPosition() + 1)
            elif stream.HasNextN(2) && stream.NextN(2) = [|'\r'; '\n'|] then
                Succsess ((), c, stream.GetPosition() + 2)
            else
                Failure ("Expected a new line.", Some (stream.GetPosition()))
        else
            Failure ("Expected a new line but the stream ends.", None)
    |> Parser


let anyChar () =
    fun (stream: CharStream<_>) ->
        if not stream.HasNext then
            let message = sprintf "Expected a char but the input stream is empty."
            Failure (message, None)
        else
            Succsess (stream.Next, stream.GetContext(), stream.GetPosition() + 1)
    |> Parser


let anyOf (chars: char Set) =
    fun (stream: CharStream<_>) ->
        if not stream.HasNext then
            let message = sprintf "Expected anything of %A but the input stream is empty." chars
            Failure (message, None)
        else 
            let nextc = stream.Next
            if not <| Set.contains nextc chars then
                let message = sprintf "Expected anything of: %A but instead found %c." chars nextc
                Failure (message, Some(stream.GetPosition()))
            else
                Succsess (nextc, stream.GetContext(), stream.GetPosition() + 1)
    |> Parser


let pchar c =
    fun (stream: CharStream<_>) ->
        if not stream.HasNext then
            let message = sprintf "Expected: %c but the input stream is empty." c
            Failure (message, None)
        else 
            let nextc = stream.Next
            if nextc <> c then
                let message = sprintf "Expected: %c but instead found %c." c nextc
                Failure (message, Some(stream.GetPosition()))
            else
                Succsess (c, stream.GetContext(), stream.GetPosition() + 1)
    |> Parser


let pstring (str: string) =
    fun (stream: CharStream<_>) ->
        if not <| stream.HasNextN(str.Length) then
            let message = sprintf "Expected: %s but the input stream ends." str
            Failure (message, None)
        else 
            let nextstr = stream.NextN(str.Length) |> String.Concat
            if nextstr <> str then
                let message = sprintf "Expected: \"%s\" but instead found \"%s\"." str nextstr
                Failure (message, Some (stream.GetPosition()))
            else
                Succsess (str, stream.GetContext(), stream.GetPosition() + str.Length)
    |> Parser

