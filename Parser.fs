module Parser

open System
open ParserCombinators
open LanguageModel


type JuriContext =
    {
        InFunctionDefinition : bool
        InLoop : bool
        IndentationStack : int list
        Variables : Identifier list
        Functions : Identifier list
    }
    static member Default =
        {
            InFunctionDefinition = false
            InLoop = false
            IndentationStack = [0]
            Variables = []
            Functions = []
        }


let parserErrorPrinter (stream: CharStream<'c>) msg (error: ParserError option) =
    let sourceLine, errorPos =
        match error with
        | Some x ->
            let lineStart = stream.GetChars.[..x] |> Array.findIndexBack (fun c -> c = '\r' || c = '\n')
            let lineEnd = stream.GetChars.[x..] |> Array.findIndex (fun c -> c = '\r' || c = '\n')
            let line =
                stream.GetChars.[lineStart + 1 .. lineEnd - 1]
                |> String.Concat
            (line, x - lineStart)
        | None -> ("", 0)

    Console.ForegroundColor <- ConsoleColor.Red
    let space = String.replicate errorPos " " 
    printfn ""
    printfn "%s" sourceLine
    printfn "%s^" space
    printfn "Error: %s" msg
    printfn ""
    Console.ResetColor()



let private ws =
    many (pchar ' ' <|> pchar '\t')


let EOS = createEOS<JuriContext> ()
let newline = createNewline<JuriContext> ()
let newlineEOS = either newline EOS


// identifier

let private identifierStart =
    '_' :: ['a'..'z'] @ ['A'..'Z']
    |> List.map pchar
    |> choice

let private identifierTail =
    '_' :: ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
    |> List.map pchar
    |> choice


let private identifier =
    identifierStart .>>. (many identifierTail)
    .>> ws
    |>> fun (c, cs) -> c :: cs |> String.Concat |> Identifier


// keywords and controll chars
let eq =
    pchar '=' .>> ws

let ifloop =
    pstring "if" .>> ws

let def =
    pstring "def" .>> ws

let openParen =
    pchar '(' .>> ws

let closingParen =
    pchar ')' .>> ws


// expressions

let private expression, expressionImpl = createParserForwarder ()


// number
let private punctuation = pchar '.'
let private sign = pchar '-' <|> pchar '+'

let private digit =
    "0123456789"
    |> Seq.map pchar
    |> choice

let private number = 
    (optional sign) .>>. (many1 digit) .>>. (optional punctuation) .>>. (many digit)
    |>> fun (((s,ds),p),ds2) -> s@ds@p@ds2 |> List.map string |> List.reduce ( + ) |> float |> LiteralNumber
    .>> ws
    |> deferr "Expected a Number!"


// variable reference
let private variableReference =
    identifier
    |>> VariableReference


// function call
let private functionCall =
    identifier .>> openParen .>>. (many1 expression) .>> closingParen
    |>> FunctionCall


// expression
expressionImpl :=
    functionCall
    <|> variableReference
    <|> number
    .>> ws
    |> deferr "Kein Ausdruck gefunden."


// instructions
let private instruction, instructionImpl = createParserForwarder ()



let emptyLines =
    many (ws >>. newline)



let private codeblock =

    let indentation =
        ws |>> List.length

    let indentedInstruction =
        indentation .>>. instruction

    let codeblockStart =
        indentedInstruction |> satisfies (fun (level,_) c -> level > c.IndentationStack.Head)
        |> updateContext (fun (level,_) c -> {c with IndentationStack = level :: c.IndentationStack})
        |>> snd

    let codeblockRest =
        indentedInstruction |> satisfies (fun (level,_) c -> level = c.IndentationStack.Head)
        |>> snd
    
    codeblockStart .>>. many (codeblockRest)
    |>> join2
    |> updateContext (fun _ c -> {c with IndentationStack = c.IndentationStack.Tail})



let private instructionExpression =
    expression
    .>> newlineEOS .>> emptyLines
    |>> Expression



let private assignment =
    identifier .>> eq .>>. expression
    .>> newlineEOS .>> emptyLines
    |>> fun (id, exp) -> Assignment (id, exp)



let private functionDefinition =
    def >>. identifier .>>. (many1 identifier)
    .>> newlineEOS .>> emptyLines
    .>>. codeblock
    |>> fun ((id, argNames), body) -> FunctionDefinition (id, argNames, body)



let private loop =
    ifloop >>. expression
    .>> newlineEOS .>> emptyLines
    .>>. codeblock
    |>> fun (con, body) -> Loop (con, body)



instructionImpl :=
    [loop; functionDefinition; assignment; instructionExpression;]
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
        parserErrorPrinter stream m e
    | Succsess(r,_,_) ->
        printfn "%A" r
        ()
    prog
