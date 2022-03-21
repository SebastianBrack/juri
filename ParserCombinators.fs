module Juri.Internal.ParserCombinators


open System


type Position = int

type ParserError = Position * Position


type ParserResult<'ResultType, 'ParserContext> =
    | Success of 'ResultType * 'ParserContext * Position 
    | Failure of string * ParserError option
    | Fatal   of string * ParserError option

let isSuccess = function | Success _ -> true  | Failure _ -> false | Fatal _ -> false
let isFailure  = function | Success _ -> false | Failure _ -> true  | Fatal _ -> true
let isFatal    = function | Success _ -> false | Failure _ -> false | Fatal _ -> true


type Parser<'T, 'ParserContext> = Parser of (CharStream<'ParserContext> -> ParserResult<'T, 'ParserContext>)

and CharStream<'ParserContext>(charseq: char seq, context: 'ParserContext) =
    let content = Seq.toArray charseq 
    let mutable parserContext = context
    let mutable position : Position = 0

    
    member this.PrintError(m, perror) =
        let findLine pos =
            let lineStartOPT = content.[..pos - 1] |> Array.tryFindIndexBack (fun c -> c = '\r' || c = '\n')
            let lineEndOPT = content.[pos..] |> Array.tryFindIndex (fun c -> c = '\r' || c = '\n')

            let lineStart =
                match lineStartOPT with
                | Some x -> x + 1
                | None   -> 0

            let lineEnd =
                match lineEndOPT with
                | Some x -> x + pos - 1
                | None   -> content.Length - 1

            let line =
                content.[lineStart .. lineEnd]
                |> String.Concat
            (line, pos - lineStart)

        let (sourceLine, errorPos), markLength =
            match perror with
            | Some x ->
                (findLine (fst x), (snd x - fst x))
            | None   ->
                (findLine (position), 1)

        Console.ForegroundColor <- ConsoleColor.Red
        //eprintfn "spache: %i mark: %i" errorPos markLength
        let space = String.replicate (errorPos) " " 
        let mark  = String.replicate markLength "^"
        printfn ""
        printfn "%s" sourceLine
        printfn "%s%s" space mark
        printfn "Error: %s" m
        printfn ""
        Console.ResetColor()


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
        | Success (r,c,p) -> Success (mapper r, c, p)
        | Failure (m,e)    -> Failure (m,e)
        | Fatal (m,e)      -> Fatal (m,e)
    |> Parser

/// Infix version of map.
let ( |>> ) parser mapper = map mapper parser



/// Maps the result and the context of a parser according to the mapper.
let mapc mapper parser =
    fun stream ->
        match run parser stream with
        | Success (r,c,p) -> Success (mapper r c, c, p)
        | Failure (m,e)    -> Failure (m,e)
        | Fatal (m,e)      -> Fatal (m,e)
    |> Parser
    
/// infix version of mapc
let ( ||>> ) parser mapper = mapc mapper parser


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
        | Success (r,c,p) -> Success (r,c,p)
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
        | Success (r,c,p) ->
            let newContext = f r c
            stream.SetContext(newContext)
            Success (r, newContext, p)
    |> Parser



/// The overall Parser succeeds if the given predicate returns true and fails if it returns false
let satisfies predicate parser =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run parser stream with
        | Failure (m,e) ->
            Failure (m,e)
        | Fatal (m,e) ->
            Fatal (m,e)
        | Success (r,c,p) ->
            match predicate r (stream.GetContext()) with
            | false ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure ("", None)
            | true  ->
                Success (r,c,p)
    |> Parser
    
    
    
/// makes the parser not consume anything of the stream even if it succeeds.
let lookAhead parser =
    fun (stream: CharStream<'c>) ->
        let pOriginal = stream.GetPosition()
        match run parser stream with
        | Failure (m,e) ->
            Failure (m,e)
        | Fatal (m,e) ->
            Fatal (m,e)
        | Success (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(pOriginal)
            Success (r,c,pOriginal)
    |> Parser



/// Chains two parsers together and combines the results into a tupel.
let ( .>>. ) left right =
    fun (stream: CharStream<'c>) ->
        let cOriginal = stream.GetContext()
        let pOriginal = stream.GetPosition()
        match run left stream with
        | Failure (m,pe)      -> Failure (m,pe)
        | Fatal (m,pe)        -> Fatal (m,pe)
        | Success (r1,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Success (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Success ((r1,r2), c2, p2)
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
        | Success (_,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Success (r2,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Success (r2, c2, p2)
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
        | Success (r1,c1,p1) ->
            stream.SetContext(c1)
            stream.SetPosition(p1)
            match run right stream with
            | Failure (m,pe) ->
                stream.SetContext(cOriginal)
                stream.SetPosition(pOriginal)
                Failure (m,pe)
            | Fatal (m,pe) ->
                Fatal (m,pe)
            | Success (_,c2,p2) ->
                stream.SetContext(c2)
                stream.SetPosition(p2)
                Success (r1, c2, p2)
    |> Parser


/// Combines two parsers into a new parser that first tries to apply the
/// first parser and if it fails applies the second parser.
let either left right =
    fun (stream: CharStream<'c>) ->
        match run left stream with
        | Success (r1,c1,p1) as s1 ->
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
            | Success (r2,c2,p2) as s2 ->
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
            | Success (_) as s -> Some s
            | Fatal (_) as f    -> Some f
            | Failure (_)       -> None
        match Seq.tryPick succseedingOrFatalParser parsers with
        | None                    -> Failure ("Nothing parsable was found in the input stream.", None)
        | Some (Fatal (m,e))      -> Fatal (m,e)
        | Some (Success (r,c,p)) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            Success (r,c,p)
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
            Success (results, stream.GetContext(), stream.GetPosition()) 
        | Success (r,c,p) ->
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
            Success (results, stream.GetContext(), stream.GetPosition()) 
        | Success (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            innerFnc (results @ [r]) stream
    Parser (innerFnc [])


/// Tries to Parse a list of As that ends with a B
/// If Parser A fails before a B can be parsed the overall Parser fails.
let AsUntilB parserA parserB =
    let rec innerFnc results stream =
        match run parserB stream with
        | Fatal (m,e) ->
            Fatal (m,e)
        | Failure (_) ->
            match run parserA stream with
            | Fatal (m,e) ->
                Fatal (m,e)
            | Failure (m,pe) ->
                Failure (m,pe)
            | Success (r,c,p) ->
                stream.SetContext(c)
                stream.SetPosition(p)
                innerFnc (results @ [r]) stream
        | Success (_,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            Success (results, stream.GetContext(), stream.GetPosition()) 
    Parser (innerFnc [])
        


/// Applies the parser. If it succeeds it wrappes the result in a list.
/// If it fails it returns an empty list as result.
let optional parser =
    fun stream ->
        match run parser stream with
        | Success (r,c,p) ->
            stream.SetContext(c)
            stream.SetPosition(p)
            Success ([r],c,p)
        | Failure (m,pp) ->
            Success ([], stream.GetContext(), stream.GetPosition())
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
        let pos = stream.GetPosition()
        if not stream.HasNext then
            Success ((), c, pos)
        else
            Failure ("End of stream expected but there is more stuff.", None)
    |> Parser



let createNewline<'c> () =
    fun (stream: CharStream<_>) ->
        let c: 'c = stream.GetContext()
        let pos = stream.GetPosition()
        if stream.HasNext then
            let nextc = stream.Next
            if nextc = '\n' || nextc = '\r' || nextc = '\u0085' || nextc = '\u2029' || nextc = '\uffff' then
                Success ((), c, pos + 1)
            elif stream.HasNextN(2) && stream.NextN(2) = [|'\r'; '\n'|] then
                Success ((), c, pos + 2)
            else
                Failure ("Expected a new line.", None)
        else
            Failure ("Expected a new line but the stream ends.", None)
    |> Parser


let anyChar () =
    fun (stream: CharStream<_>) ->
        let pos = stream.GetPosition()
        if not stream.HasNext then
            let message = sprintf "Expected a char but the input stream is empty."
            Failure (message, None)
        else
            Success (stream.Next, stream.GetContext(), pos + 1)
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
                Failure (message, None)
            else
                Success (nextc, stream.GetContext(), stream.GetPosition() + 1)
    |> Parser


let anyBut (charsToExclude: char Set) =
    fun (stream: CharStream<_>) ->
        if not stream.HasNext then
            let message = sprintf "Expected anything that is not %A but the input stream is empty." charsToExclude
            Failure (message, None)
        else 
            let nextc = stream.Next
            if Set.contains nextc charsToExclude then
                let message = sprintf "Expected anything that is not: %A but instead found %c." charsToExclude nextc
                Failure (message, None)
            else
                Success (nextc, stream.GetContext(), stream.GetPosition() + 1)
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
                Failure (message, None)
            else
                Success (c, stream.GetContext(), stream.GetPosition() + 1)
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
                Failure (message, None)
            else
                Success (str, stream.GetContext(), stream.GetPosition() + str.Length)
    |> Parser

