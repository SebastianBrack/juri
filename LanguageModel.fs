module Internal.LanguageModel


type Identifier = Identifier of string
type BinaryOperator = BinaryOperator of string
type ListIdentifier = ListIdentifier of string

type Expression =
    | LiteralNumber of float
    | LiteralList of Expression list
    | ListReference of ListIdentifier
    | ListAccess of ListIdentifier * Expression
    | VariableReference of Identifier
    | FunctionCall of functionName: Identifier * arguments: Expression list
    | Binary of operator: BinaryOperator * left: Expression * right: Expression

type Instruction =
    | Expression of Expression
    | Assignment of variableName: Identifier * value: Expression
    | ListAssignment of ListName: ListIdentifier * values: Expression list
    | ListElementAssignment of  ListIdentifier * Expression * Expression
    | FunctionDefinition of functionName: Identifier * argumentNames: Identifier list * functionBody: Codeblock
    | Loop of condition: Expression * repeat: bool * loopBody: Instruction list
    | OperatorDefinition of operator: BinaryOperator * leftArg: Identifier * rightArg: Identifier * functionBody: Codeblock


and Codeblock = Instruction list