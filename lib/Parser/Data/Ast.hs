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
    EnumT (..),
    Mutablility (..),
    FunctionName,
    VariableName,
    Body,
  )
where

import Data.List(intercalate)

newtype Program = Program [Statement]
  deriving (Show, Eq)

type VariableName = String

type FunctionName = String

type Field = (VariableName, Type) -- <field_name>: <field_type>

type Body = [Statement]

data Statement
  = VariableDeclaration VariableName Type (Maybe Expression) -- let <name>: <type> = <expression>
  | FunctionDeclaration FunctionName [Field] Type Body -- fn <name>(<args>) -> <type> { <body> }
  | WhileLoop Expression Body -- while <condition> { <body> }
  | If Expression Body (Maybe Body) -- if <cond> { <then_body> } [else { <else_body> }] NOTE: 'elif <cond> { }' is actually else '{ if <cond> { <then_body> } }'
  | TypeDeclaration String Type -- type <name> = <typedef>
  | ReturnStatement Expression -- return <expression>
  | StandaloneFunctionCall FunctionName [Expression] -- <func_name>(<args>)
  | VariableReAssignment VariableName Expression -- <name> = <expression>
  deriving (Show, Eq)

data Mutablility
  = Mutable
  | Immutable
  deriving (Show, Eq)

data Type
  = PrimitiveType !Mutablility !Primitive -- i8, u8, f32, etc.
  | PointerType !Mutablility Type -- `\*mut or *` <type>
  | StructType !Mutablility !Struct -- { <name>: <type>, ... }
  | ArrayType !Mutablility !Array -- [<size>: <type>]
  | EnumType !Mutablility !EnumT -- <variant1, variant2, ...>
  | CustomType !Mutablility !String -- <name> (Must start with a capital letter)
  deriving (Eq)

instance Show Type where
  show (PrimitiveType m p) = show m ++ " " ++ show p
  show (PointerType m t) = show m ++ " " ++ show t
  show (StructType m s) = show m ++ " " ++ show s
  show (ArrayType m a) = show m ++ " " ++ show a
  show (EnumType m e) = show m ++ " " ++ show e
  show (CustomType m n) = show m ++ " " ++ show n

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
  | ELiteral !Literal -- constant values
  | BinaryOp !BinOp Expression Expression -- <expr> <op> <expr>
  | UnaryOp !UnaryOp Expression -- <op> <expr>
  | Parenthesis Expression -- (<expr>)
  | FunctionCall FunctionName [Expression] -- <func_name>(<args>)
  deriving (Eq)

instance Show Expression where
  show (Variable n) = show n
  show (ELiteral l) = show l
  show (BinaryOp op l r) = show l ++ " " ++ show op ++ " " ++ show r
  show (UnaryOp op ex) = show op ++ " " ++ show ex
  show (Parenthesis ex) = show "(" ++ show ex ++ show ")"
  show (FunctionCall n a) = show n ++ "(" ++ intercalate ", " (map show a) ++ show ")"

data Literal
  = -- | StringLiteral String
    IntLiteral !Integer
  | FloatLiteral !Double
  deriving
    (Eq)

instance Show Literal where
  show (IntLiteral n) = show n
  show (FloatLiteral n) = show n

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
  | Negative -- -
  deriving (Show, Eq)

-- Struct
newtype Struct = Struct [Field] -- { <fields> }
  deriving (Show, Eq)

-- Array
data Array = Array !Int !Type -- [<size>: <type>]
  deriving (Show, Eq)

newtype EnumT = EnumT [String] -- <variant1, variant2, ...>
  deriving (Eq)

instance Show EnumT where
  show (EnumT v) = "Enum { " ++ intercalate ", " v ++ " }"