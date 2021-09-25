module Parser

open System
open ParserCombinators
open LanguageModel


let parserErrorPrinter stream msg =
    Console.ForegroundColor <- ConsoleColor.Red
    //let space = [ for _ in 0 .. (stream.Position - 1) -> " " ] |> String.Concat
    printfn ""
    //printfn "%s" (stream.Content |> String.Concat)
    //printfn "%s^" space
    printfn "Error: %s" msg
    printfn ""
    Console.ResetColor()



let private ws =
    many (pchar ' ')


let EOS = createEOS<unit> ()
let newline = createNewline<unit> ()
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

let blockEnd =
    pstring "end" .>> ws .>> newlineEOS

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

let private codeblock =
    many1 (instruction) .>> blockEnd
    |> deferr "Der Codeblock ist nicht vollständig"

let private instructionExpression =
    expression
    .>> newlineEOS
    |>> Expression

let private assignment =
    identifier .>> eq .>>. expression
    .>> newlineEOS
    |>> fun (id, exp) -> Assignment (id, exp)

let private functionDefinition =
    def >>. identifier .>>. (many1 identifier)
    .>> newline
    .>>. codeblock
    |>> fun ((id, argNames), body) -> FunctionDefinition (id, argNames, body)

let private loop =
    ifloop >>. expression
    .>> newline
    .>>. codeblock
    |>> fun (con, body) -> Loop (con, body)

let private programm =
    many1 instruction

instructionImpl :=
    [loop; functionDefinition; assignment; instructionExpression;]
    |> choice
    |> deferr "Keine Anweisungen gefunden."




let parseProgramm (text: string) =
    let stream = CharStream(text, ())
    let prog = stream.RunParser(programm)
    match prog with
    | Failure (m,_) ->
        parserErrorPrinter stream m
        printfn "%A" stream
    | _ -> ()
    eprintfn "Parsed Programm: %A" prog
    prog
