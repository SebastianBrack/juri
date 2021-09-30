module LanguageModel


type Identifier = Identifier of string
type BinaryOperator = BinaryOperator of string


type Expression =
    | LiteralNumber of float
    | VariableReference of Identifier
    | FunctionCall of functionName: Identifier * arguments: Expression list
    | Binary of operator: BinaryOperator * left: Expression * right: Expression


type Instruction =
    | Expression of Expression
    | Assignment of variableName: Identifier * value: Expression
    | FunctionDefinition of functionName: Identifier * argumentNames: Identifier list * functionBody: Codeblock
    | Loop of condition: Expression * repeat: bool * loopBody: Instruction list
    | OperatorDefinition of operator: BinaryOperator * leftArg: Identifier * rightArg: Identifier * functionBody: Codeblock


and Codeblock = Instruction list