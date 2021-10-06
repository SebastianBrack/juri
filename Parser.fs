module Parser

open System
open ParserCombinators
open LanguageModel

type IndentationType =
    | Tabs
    | Spaces
    | Unknown

type JuriContext =
    {
        IndentationType : IndentationType
        InFunctionDefinition : bool
        InLoop : bool
        IndentationStack : int list
        Variables : Identifier list
        Functions : Identifier list
    }
    static member Default =
        {
            IndentationType = Unknown
            InFunctionDefinition = false
            InLoop = false
            IndentationStack = [0]
            Variables = []
            Functions = []
        }



let private ws =
    many (pchar ' ' <|> pchar '\t')

let newline = createNewline<JuriContext> ()
let EOS = createEOS<JuriContext>()
let newlineEOS = either newline EOS



let private identifier =

    let identifierStart =
        '_' :: ['a'..'z'] @ ['A'..'Z']
        |> Set |> anyOf

    let identifierTail =
        '_' :: ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
        |> Set |> anyOf

    identifierStart .>>. (many identifierTail)
    .>> ws
    |>> fun (c, cs) -> c :: cs |> String.Concat |> Identifier
    |> deferr "Es wurde ein Identifier erwartet."



let private operator =

    let operatorChar =
        ['+'; '-'; '*'; '/'; '>'; '<'; '.'; '='; '!']
        |> Set |> anyOf

    many1 operatorChar
    .>> ws
    |>> (String.Concat >> BinaryOperator)



// keywords and controll chars
let eq =
    pchar '=' .>> ws

let ifloop =
    pstring "if" .>> ws

let repeat =
    optional (pstring "repeat") .>> ws |>> (function | [] -> false | _ -> true)

let jfun =
    pstring "fun" .>> ws

let binary =
    pstring "operator" .>> ws

let openParen =
    pchar '(' .>> ws

let closingParen =
    pchar ')' .>> ws |> deferr "Es fehlt eine schließende Klammer"


// expressions
let private expression, expressionImpl = createParserForwarder ()



let private number = 

    let punctuation = pchar '.'
    let sign = pchar '-' <|> pchar '+'

    let digit =
        "0123456789"
        |> Set |> anyOf

    (optional sign) .>>. (many1 digit) .>>. (optional punctuation) .>>. (many digit)
    |>> fun (((s,ds),p),ds2) -> s@ds@p@ds2 |> List.map string |> List.reduce ( + ) |> float |> LiteralNumber
    .>> ws
    |> deferr "Eine Zahl wurde erwartet."



let private variableReference =
    identifier
    |>> VariableReference



let private functionCall =
    identifier .>> openParen .>>. (many1 expression)
    .>> (closingParen |> failAsFatal)
    |>> FunctionCall



let private binaryOperation =
    (number <|> variableReference <|> functionCall) .>>. operator .>>. expression
    |>> fun ((left, op), right) -> Binary (op, left, right)



expressionImpl :=
    [binaryOperation; functionCall; variableReference; number]
    |> choice
    .>> ws
    |> deferr "Es wird ein Ausdruck erwartet."



// instructions
let private instruction, instructionImpl = createParserForwarder ()



let emptyLines =
    many (ws >>. newline)



let private codeblock =

    let countTabsAndSpaces (chars: char list) =
        let mutable tabs = 0
        let mutable spaces = 0
        chars |> List.iter (function | '\t' -> tabs <- tabs + 1 | ' ' -> spaces <- spaces + 1 | _ -> ())
        (tabs, spaces)


    let indentation =
        fun (stream: CharStream<JuriContext>) ->
            let posStart = stream.GetPosition()
            match run (ws |>> countTabsAndSpaces) stream with
            | Failure (m,e) -> Failure (m,e)
            | Fatal (m,e)   -> Fatal (m,e)
            | Succsess ((tabs,spaces),c,p) ->
                eprintfn "parsed indentation -> tabs: %i spaces: %i" tabs spaces
                match (tabs, spaces, c.IndentationType) with
                | (t,0,Unknown) ->
                    let newContext = {stream.GetContext() with IndentationType = Tabs}
                    stream.SetPosition(p)
                    Succsess (t,newContext,p)
                | (t,0,Tabs) ->
                    stream.SetPosition(p)
                    Succsess (t,c,p)
                | (0,s,Unknown) ->
                    let newContext = {stream.GetContext() with IndentationType = Spaces}
                    stream.SetPosition(p)
                    Succsess (s,newContext,p)
                | (0,s,Spaces) ->
                    stream.SetPosition(p)
                    Succsess (s,c,p)
                | (t,s,_) ->
                    let errorMark = (posStart, t + s)
                    Fatal ("Tabs und Leerzeichen dürfen nicht gemischt werden.", Some errorMark)
        |> Parser


    let codeblockHead =
        indentation .>>. instruction
        |> satisfies (fun (level,_) c -> level > c.IndentationStack.Head)
        |> updateContext (fun (level,_) c -> {c with IndentationStack = level :: c.IndentationStack})
        |>> snd

    let codeblockTail =
        indentation |> satisfies (fun level c -> level = c.IndentationStack.Head)
        >>. instruction
    
    codeblockHead .>>. (many codeblockTail)
    |>> join2
    |> updateContext (fun _ c -> {c with IndentationStack = c.IndentationStack.Tail})
    |> deferr "Es fehlt ein Codeblock."



let private instructionExpression =
    expression
    .>> newlineEOS .>> emptyLines
    |>> Expression



let private assignment =
    identifier
    .>> eq
    .>>. (expression |> failAsFatal)
    .>> newlineEOS .>> emptyLines
    |>> fun (id, exp) -> Assignment (id, exp)



let private functionDefinition =
    jfun
    >>. (identifier |> failAsFatal)
    .>>. ((many1 identifier) |> failAsFatal)
    .>> newline .>> emptyLines
    .>>. (codeblock |> failAsFatal)
    |>> fun ((id, argNames), body) -> FunctionDefinition (id, argNames, body)



let private binaryOperatorDefinition =
    binary
    >>. (operator |> failAsFatal)
    .>>. (identifier |> failAsFatal)
    .>>. (identifier |> failAsFatal)
    .>> newline .>> emptyLines
    .>>. (codeblock |> failAsFatal)
    |>> fun (((op, left), right), body) -> OperatorDefinition (op, left, right, body)



let private loop =
    ifloop
    >>. (expression |> failAsFatal)
    .>>. repeat
    .>> newline .>> emptyLines
    .>>. (codeblock |> failAsFatal)
    |>> fun ((con, rep), body) -> Loop (con, rep, body)



instructionImpl :=
    [binaryOperatorDefinition; loop; functionDefinition; assignment; instructionExpression;]
    |> choice



let private programm =
    emptyLines
    >>. many1 instruction
    .>> emptyLines
    .>> EOS



let parseProgramm (text: string) =
    let stream = CharStream(text, JuriContext.Default)
    let prog = stream.RunParser(programm)
    match prog with
    | Failure (m,e) ->
        stream.PrintError(m,e)
    | Fatal (m,e) ->
        stream.PrintError(m,e)
    | Succsess(r,_,_) ->
        //printfn "%A" r
        //printfn "%A" (stream.GetContext())
        ()
    prog
