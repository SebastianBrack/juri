module Juri.Internal.Parser

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



let digit =
        "0123456789"
        |> Set |> anyOf

let integer =
    many1 digit 
    //|>> fun cs -> cs |> List.map string |> List.reduce ( + ) |> int
    |>> (String.Concat >> int)
    |> deferr "Listenelemente müssen mit einer Ganzahl Indexiert werden."

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



let private listIdentifier =

    let identifierStart = pchar ':' 

    let identifierTail =
        '_' :: ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
        |> Set |> anyOf

    identifierStart .>>. (many identifierTail)
    .>> ws
    |>> fun (c, cs) -> c :: cs |> String.Concat |> Identifier
    |> deferr "Es wurde ein ListIdentifier erwartet."




let private operator =

    let operatorChar =
        ['+'; '-'; '*'; '/'; '>'; '<'; '.'; '='; '!'; '%']
        |> Set |> anyOf

    many1 operatorChar
    .>> ws
    |>> (String.Concat >> BinaryOperator)



// keywords and controll chars
let eq =
    pchar '=' .>> ws
    
let rangeOperator =
    pstring "to" .>> ws
    
let jinit =
    pstring "init" .>> ws
    
let jbreak =
    pstring "break" .>> ws
    
let jreturn =
    pstring "return" .>> ws
    
let iterate =
    pstring "iterate" .>> ws
    
let jas =
    pstring "as" .>> ws

let ifloop =
    pstring "if" .>> ws
    
let jthen =
    pstring "then" .>> ws

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

let openBracket =
    pchar '[' .>> ws |> deferr "Es fehlt eine öffnende Klammer"

let closingBracket =
    pchar ']' .>> ws |> deferr "Es fehlt eine schließende Klammer"

// expressions
let private expression, expressionImpl = createParserForwarder ()



let private number = 

    let punctuation = pchar '.'
    let sign = pchar '-' <|> pchar '+'

    (optional sign) .>>. (many1 digit) .>>. (optional punctuation) .>>. (many digit)
    |>> fun (((s,ds),p),ds2) -> s@ds@p@ds2 |> List.map string |> List.reduce ( + ) |> float |> LiteralNumber
    .>> ws
    |> deferr "Eine Zahl wurde erwartet."



let private parenthesizedExpression =
    openParen
    >>. expression
    .>> (closingParen |> failAsFatal)
    |>> ParenthesizedExpression
    


let private variableReference =
    identifier
    |>> VariableReference



//let private listReference =
//    listIdentifier
//    |>> ListReference



let private functionCall =
    identifier .>> openParen .>>. (many expression)
    .>> (closingParen |> failAsFatal)
    |>> FunctionCall



let private listLength =
    pchar '?' >>. listIdentifier
    |>> ListLength
    
    
    
let private listAccess =
    (parenthesizedExpression <|> number <|> variableReference <|> functionCall <|> listLength) .>>. listIdentifier
    |>> fun (index, id) -> ListAccess (id, index)


let private binaryOperation =
    (parenthesizedExpression <|> listAccess <|> listLength <|> number <|> variableReference <|> functionCall )
    .>>. operator
    .>>. (expression |> deferr "Es fehlt ein Operand." |> failAsFatal)
    |>> fun ((left, op), right) -> Binary (op, left, right)
// 1 + 2 + 3 + 4
// (1+2) + 3
// 1 + (2+ (3+4))


expressionImpl.Value <-
    [
        binaryOperation
        parenthesizedExpression
        listAccess
        functionCall
        variableReference
        listLength
        number
    ]
    |> choice
    .>> ws
    |> deferr "Es wird ein Ausdruck erwartet."



//let private listLiteral = 
//   openBracket >>. (many expression) .>> closingBracket
//    |>> LiteralList



// instructions
let private instruction, instructionImpl = createParserForwarder ()



let emptyLines =
    let commentLine =
        ws >>. pchar '#' .>> (AsUntilB (anyChar()) newlineEOS)
        |>> ignore
    let empty =
        ws >>. newline
        |>> ignore
    many (either commentLine empty)



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
            | Success ((tabs,spaces),c,p) ->
                match (tabs, spaces, c.IndentationType) with
                | (t,0,Unknown) ->
                    let newContext = {stream.GetContext() with IndentationType = Tabs}
                    stream.SetPosition(p)
                    Success (t,newContext,p)
                | (t,0,Tabs) ->
                    stream.SetPosition(p)
                    Success (t,c,p)
                | (0,s,Unknown) ->
                    let newContext = {stream.GetContext() with IndentationType = Spaces}
                    stream.SetPosition(p)
                    Success (s,newContext,p)
                | (0,s,Spaces) ->
                    stream.SetPosition(p)
                    Success (s,c,p)
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



let private listAssignment =
    listIdentifier
    .>> eq
    .>>. (openBracket >>. (many expression) .>> closingBracket)
    .>> newlineEOS .>> emptyLines
    |>> ListAssignment
    
    
    
let private listAssignmentWithRange = // has to be tried before listAssignment in the parsing order
    listIdentifier
    .>> eq
    .>>. (openBracket >>. expression .>> rangeOperator .>>. expression .>> closingBracket)
    .>> newlineEOS .>> emptyLines
    |>> fun (id, (lowerBound, upperBound)) -> ListAssignmentWithRange (id, lowerBound, upperBound)
    
    
    
let private listInitialisationWithCode = // has to be tried before listInitialisationWithValue in the parsing order
    listIdentifier
    .>> eq
    .>> jinit
    .>>. (expression |> failAsFatal)
    .>> jas
    .>>. (identifier |> failAsFatal)
    .>> newline .>> emptyLines
    .>>. (codeblock |> failAsFatal)
    |>> fun (((id, size), indexName), body) -> ListInitialisationWithCode (id, size, indexName, body)
    
    
    
let private listInitialisationWithValue =
    listIdentifier
    .>> eq
    .>> jinit
    .>>. (expression |> failAsFatal)
    .>>. (expression |> failAsFatal)
    .>> newlineEOS .>> emptyLines
    |>> fun ((id, size), value) -> ListInitialisationWithValue (id, size, value)



let private listElementAssignment =
    (number <|> variableReference <|> functionCall)
    .>>. listIdentifier
    .>> eq
    .>>. (expression |> failAsFatal)
    .>> newlineEOS .>> emptyLines
    |>> fun ((index, id), exp) -> ListElementAssignment (id, index, exp)



let private listIteration =
    iterate
    >>. (listIdentifier |> failAsFatal)
    .>> (jas |> failAsFatal)
    .>>. identifier
    .>> newline .>> emptyLines
    .>>. (codeblock |> failAsFatal)
    |>> fun ((listName, elementName), body) -> Iteration (listName, elementName, body)
    
    

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
    
    
    
let private conditionWithSingleStatement = // has to be parsed before loop
    ifloop
    >>. (expression |> failAsFatal)
    .>> jthen
    .>>. (instruction |> failAsFatal)
    |>> fun (con, statement) -> Loop (con, false, [statement])



let private breakStatement =
    jbreak
    .>> newlineEOS .>> emptyLines
    |>> (fun _ -> Break)
    


let private returnStatement =
    jreturn
    >>. expression
    .>> newlineEOS .>> emptyLines
    |>> Return



instructionImpl.Value <-
    [   binaryOperatorDefinition
        conditionWithSingleStatement
        loop
        functionDefinition
        assignment
        listAssignmentWithRange
        listAssignment
        listInitialisationWithCode
        listInitialisationWithValue
        listElementAssignment
        listIteration
        breakStatement
        returnStatement
        instructionExpression ]
    |> choice



let private program =
    emptyLines
    >>. many1 instruction
    .>> emptyLines
    .>> EOS



let parseProgram (text: char seq) =
    let stream = CharStream(text, JuriContext.Default)
    let parsingResult = stream.RunParser(program)
    match parsingResult with
    | Failure (m,e) ->
        //stream.PrintError(m,e)
        ()
    | Fatal (m,e) ->
        //stream.PrintError(m,e)
        ()
    | Success(r,_,_) ->
        //printfn "%A" r
        //printfn "%A" (stream.GetContext())
        ()
    parsingResult
