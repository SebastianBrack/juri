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



let private operator =

    let operatorChar =
        ['+'; '-'; '*'; '/'; '>'; '<'; '.']
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
    pchar ')' .>> ws


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
    |> deferr "Expected a Number!"



let private variableReference =
    identifier
    |>> VariableReference



let private functionCall =
    identifier .>> openParen .>>. (many1 expression) .>> closingParen
    |>> FunctionCall



let private binaryOperation =
    (number <|> variableReference <|> functionCall) .>>. operator .>>. expression
    |>> fun ((left, op), right) -> Binary (op, left, right)



expressionImpl :=
    [binaryOperation; functionCall; variableReference; number]
    |> choice
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
    
    codeblockStart .>>. (many codeblockRest)
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
    jfun >>. identifier .>>. (many1 identifier)
    .>> newline .>> emptyLines
    .>>. codeblock
    |>> fun ((id, argNames), body) -> FunctionDefinition (id, argNames, body)



let private binaryOperatorDefinition =
    binary >>. operator .>>. identifier .>>. identifier 
    .>> newline .>> emptyLines
    .>>. codeblock
    |>> fun (((op, left), right), body) -> OperatorDefinition (op, left, right, body)



let private loop =
    ifloop >>. expression .>>. repeat
    .>> newline .>> emptyLines
    .>>. codeblock
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
        parserErrorPrinter stream m e
    | Succsess(r,_,_) ->
        //printfn "%A" r
        ()
    prog
