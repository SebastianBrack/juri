module LanguageModel


type Identifier = Identifier of string


type Expression =
    | LiteralNumber of float
    | VariableReference of Identifier
    | FunctionCall of functionName: Identifier * arguments: Expression list


type Instruction =
    | Expression of Expression
    | Assignment of variableName: Identifier * value: Expression
    | FunctionDefinition of functionName: Identifier * argumentNames: Identifier list * functionBody: Codeblock
    | Loop of condition: Expression * repeat: bool * loopBody: Instruction list


and Codeblock = Instruction list