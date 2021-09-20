module Parser

open System
open ParserCombinators
open LanguageModel


let parserErrorPrinter { Message = msg; Stream = stream } =
    Console.ForegroundColor <- ConsoleColor.Red
    let space = [ for _ in 0 .. (stream.Position - 1) -> " " ] |> String.Concat
    printfn ""
    printfn "%s" (stream.Content |> String.Concat)
    printfn "%s^" space
    printfn "Error: %s" msg
    printfn ""
    Console.ResetColor()


let private nl = pchar '\n'


let private ws =
    many (pchar ' ')



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
    pstring "end" .>> ws .>> nl

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


// instructions
let private instruction, instructionImpl = createParserForwarder ()

let private codeblock =
    until instruction blockEnd
    |> deferr "Der Codeblock ist nicht vollständig"

let private instructionExpression =
    expression
    .>> nl
    |>> Expression

let private assignment =
    identifier .>> eq .>>. expression
    .>> nl
    |>> fun (id, exp) -> Assignment (id, exp)

let private functionDefinition =
    def >>. identifier .>>. (many1 identifier)
    .>> nl
    .>>. codeblock
    |>> fun ((id, argNames), body) -> FunctionDefinition (id, argNames, body)

let private loop =
    ifloop >>. expression
    .>> nl
    .>>. codeblock
    |>> fun (con, body) -> Loop (con, body)

let private programm =
    many1 instruction
    // |> deferr "Die Eingabe war leer."

instructionImpl :=
    [loop; functionDefinition; assignment; instructionExpression;]
    |> choice




let parseProgramm (text: string) =
    let stream = CharStream.Create text
    let prog = run programm stream
    // eprintfn "Parsed Programm: %A" prog
    prog
