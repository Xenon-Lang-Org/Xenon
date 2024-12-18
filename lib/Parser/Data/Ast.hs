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
      Array (..),
    )
where


data Program = Program [Statement]
    deriving (Show, Eq)

data Statement
    = VariableDeclaration String Type (Maybe Expression) -- let <name>: <type> = <expression>
    | FunctionDeclaration String [(String, Type)] (Maybe Type) [Statement] -- fn <name>(<args>) -> <type> { <body> }
    -- | WhileLoop Expression [Statement] -- while (<condition>) { <body> }
    -- | Conditional Expression [Statement] (Maybe Statement) -- if <cond> { <body> } [elif <cond> { <body> }] [else { <body> }]
    -- | StructDeclaration String Struct -- type <name> = { <fields> }
    -- | ArrayDeclaration String Array -- let <name>: [<size>: <type>] = <values>
    -- | EnumDeclaration String [String] -- type <name> = <variants>
    | ExpressionStatement Expression -- standalone expression
    deriving (Show, Eq)

data Type
    = PrimitiveType Primitive -- i8, u8, f32, etc.
    -- | PointerType Bool Type   -- *mut or *
    -- | StructType [Field]      -- { <name>: <type>, ... }
    -- | ArrayType Int Type      -- [<size>: <type>]
    -- | EnumType [String]       -- <variant1, variant2, ...>
    | CustomType String       -- <name> (Must start with a capital letter)
    deriving (Show, Eq)

data Primitive
    = I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | F32 | F64
    deriving (Show, Eq)

type Field = (String, Type) -- <field_name>: <field_type>

-- Expressions
data Expression
    = Variable String                     -- variable name
    | ELiteral Literal                     -- constant values
    | BinaryOp BinOp Expression Expression -- <expr> <op> <expr>
    | UnaryOp UnaryOp Expression          -- <op> <expr>
    | FunctionCall String [Expression]    -- <func_name>(<args>)
    deriving (Show, Eq)

data Literal
    = IntLiteral Int
    | FloatLiteral Float
    -- | StringLiteral String
    deriving (Show, Eq)

data BinOp
    = Add   -- +
    | Sub   -- -
    | Mul   -- *
    | Div   -- /
    | Eq    -- ==
    | Neq   -- !=
    | Lt    -- <
    | Gt    -- >
    | Le    -- <=
    | Ge    -- >=
    | And   -- &&
    | Or    -- ||
    deriving (Show, Eq)

data UnaryOp
    = Dereference -- @
    | AddressOf -- &
    | Negate -- !
    deriving (Show, Eq)

-- Struct
-- newtype Struct = [Field]

-- Array
data Array = Array Int [Expression] -- [<size>: <values>]
    deriving (Show, Eq)
