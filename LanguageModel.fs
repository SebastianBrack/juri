module Juri.Internal.LanguageModel


type Identifier = Identifier of string
type BinaryOperator = BinaryOperator of string

type Expression =
    | LiteralNumber of float
    | ListAccess of Identifier * Expression
    | ListLength of Identifier
    | VariableReference of Identifier
    | FunctionCall of functionName: Identifier * arguments: Expression list
    | Binary of operator: BinaryOperator * left: Expression * right: Expression

type Instruction =
    | Expression of Expression
    | Assignment of variableName: Identifier * value: Expression
    | ListAssignment of listName: Identifier * values: Expression list
    | ListAssignmentWithRange of listName: Identifier * lowerBound: Expression * upperBound: Expression
    | ListInitialisationWithValue of listName: Identifier * size: Expression * value: Expression
    | ListInitialisationWithCode of listName: Identifier * size: Expression * indexName: Identifier * body: Instruction list
    | ListElementAssignment of  listName: Identifier * index: Expression * value: Expression
    | FunctionDefinition of functionName: Identifier * argumentNames: Identifier list * functionBody: Codeblock
    | Loop of condition: Expression * repeat: bool * loopBody: Instruction list
    | Iteration of list: Identifier * elementName: Identifier * loopBody: Instruction list
    | OperatorDefinition of operator: BinaryOperator * leftArg: Identifier * rightArg: Identifier * functionBody: Codeblock


and Codeblock = Instruction list