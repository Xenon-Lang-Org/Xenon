module Parser.Data.Ast
  ( Program (..),
    Statement (..),
    Type (..),
    Primitive (..),
    Field,
    Expression (..),
    Literal (..),
    BinOp (..),
    UnaryOp (..),
    Struct (..),
    Array (..),
    TypeDefinition (..),
    Mutablility (..),
    FunctionName,
    VariableName,
    Body,
  )
where

import Data.Word (Word64)

newtype Program = Program [Statement]
  deriving (Show)

type VariableName = String

type FunctionName = String

type Field = (VariableName, Type) -- <field_name>: <field_type>

type Body = [Statement]

data Statement
  = VariableDeclaration VariableName Type (Maybe Expression) -- let <name>: <type> = <expression>
  | FunctionDeclaration FunctionName [Field] Type Body -- fn <name>(<args>) -> <type> { <body> }
  | WhileLoop Expression Body -- while (<condition>) { <body> }
  | If Expression Body (Maybe Body) -- if <cond> { <then_body> } [else { <else_body> }] NOTE: 'elif <cond> { }' is actually else '{ if <cond> { <then_body> } }'
  | TypeDeclaration !TypeDefinition -- type <name> = <typedef>
  | ReturnStatement Expression -- return <expression>
  | StandaloneFunctionCall FunctionName [Expression] -- <func_name>(<args>)
  | VariableReAssignment VariableName Expression -- <name> = <expression>
  deriving (Show, Eq)

data TypeDefinition
  = StructDeclaration String Struct -- { <fields> }
  | ArrayDeclaration String (Int, Type) -- [<size>: <type>]
  | EnumDeclaration String [String] -- <variant1, variant2, ...>
  | AliasDeclaration String Type -- <type>
  deriving (Show, Eq)

data Mutablility
  = Mutable
  | Immutable
  deriving (Show, Eq)

data Type
  = PrimitiveType Primitive -- i8, u8, f32, etc.
  | PointerType Mutablility Type -- `\*mut or *` <type> (True for mutable, False for immutable)
  | StructType [Field] -- { <name>: <type>, ... }
  | -- | ArrayType Int Type      -- [<size>: <type>]
    -- | EnumType [String]       -- <variant1, variant2, ...>
    CustomType String -- <name> (Must start with a capital letter)
  deriving (Show, Eq)

data Primitive
  = I8
  | I16
  | I32
  | I64
  | U8
  | U16
  | U32
  | U64
  | F32
  | F64
  deriving (Show, Eq)

data Expression
  = Variable String -- variable name
  | ELiteral Literal -- constant values
  | BinaryOp BinOp Expression Expression -- <expr> <op> <expr>
  | UnaryOp UnaryOp Expression -- <op> <expr>
  | Parenthesis Expression -- (<expr>)
  | FunctionCall FunctionName [Expression] -- <func_name>(<args>)
  deriving (Show, Eq)

data Literal
  = -- | StringLiteral String
    IntLiteral Word64
  | FloatLiteral Double
  deriving
    ( Show,
      Eq
    )

data BinOp
  = Add -- +
  | Sub -- -
  | Mul -- `*`
  | Div -- /
  | Eq -- ==
  | Neq -- !=
  | Lt -- <
  | Gt -- >
  | Le -- <=
  | Ge -- >=
  | And -- &&
  | Or -- `||`
  | Mod -- %
  | BitAnd -- `&`
  | BitOr -- `|`
  | BitXor -- `^`
  | Shl -- <<
  | Shr -- >>
  deriving (Show, Eq)

data UnaryOp
  = Dereference -- @
  | AddressOf -- ?
  | Negate -- !
  | BitNot -- ~
  deriving (Show, Eq)

-- Struct
newtype Struct = Struct [Field] -- { <fields> }
  deriving (Show, Eq)

-- Array
data Array = Array Int Type -- [<size>: <type>]
  deriving (Show, Eq)
