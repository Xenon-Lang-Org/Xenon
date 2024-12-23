module Parser.Language where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Data.Ast

-- Define Parser type

type Parser = Parsec Void String

-- Space consumer to handle whitespace
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- Lexeme parser to skip spaces after parsing a token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser to handle specific symbols
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parser for reserved keywords or identifiers
reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- Parsers for individual tokens
parseIdentifier :: Parser String
parseIdentifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

parseInt :: Parser Literal
parseInt = IntLiteral <$> lexeme L.decimal

parseFloat :: Parser Literal
parseFloat = FloatLiteral <$> lexeme L.float

parseLiteral :: Parser Literal
parseLiteral = try parseFloat <|> parseInt

-- Parse variable names and function names
variableName :: Parser String
variableName = parseIdentifier

functionName :: Parser String
functionName = parseIdentifier

-- Type parser
parseType :: Parser Type
parseType = (PrimitiveType <$> parsePrimitive) <|> (CustomType <$> parseIdentifier)
  where
    parsePrimitive = choice
      [ I8 <$ reserved "i8"
      , I16 <$ reserved "i16"
      , I32 <$ reserved "i32"
      , I64 <$ reserved "i64"
      , U8 <$ reserved "u8"
      , U16 <$ reserved "u16"
      , U32 <$ reserved "u32"
      , U64 <$ reserved "u64"
      , F32 <$ reserved "f32"
      , F64 <$ reserved "f64"
      ]

-- Expression parser
parseExpression :: Parser Expression
parseExpression = makeExprParser term operators
  where
    term = choice
      [ Variable <$> parseIdentifier
      , ELiteral <$> parseLiteral
      , parens parseExpression
      ]
    operators =
      [ [ Prefix (UnaryOp Negate <$ symbol "!") ]
      , [ InfixL (BinaryOp Mul <$ symbol "*")
        , InfixL (BinaryOp Div <$ symbol "/") ]
      , [ InfixL (BinaryOp Add <$ symbol "+")
        , InfixL (BinaryOp Sub <$ symbol "-") ]
      , [ InfixN (BinaryOp Eq <$ symbol "==")
        , InfixN (BinaryOp Neq <$ symbol "!=")
        , InfixN (BinaryOp Lt <$ symbol "<")
        , InfixN (BinaryOp Gt <$ symbol ">")
        , InfixN (BinaryOp Le <$ symbol "<=")
        , InfixN (BinaryOp Ge <$ symbol ">=") ]
      , [ InfixL (BinaryOp And <$ symbol "&&")
        , InfixL (BinaryOp Or <$ symbol "||") ]
      ]

-- Helper for parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Statement parser
parseStatement :: Parser Statement
parseStatement = choice
  [ try parseVariableDeclaration
  , try parseFunctionDeclaration
  , try parseReassignment
  , ExpressionStatement <$> parseExpression
  ]

parseVariableDeclaration :: Parser Statement
parseVariableDeclaration = do
  reserved "let"
  name <- variableName
  symbol ":"
  t <- parseType
  initializer <- optional (symbol "=" *> parseExpression)
  symbol ";"
  return $ VariableDeclaration name t initializer

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = do
  reserved "fn"
  name <- functionName
  args <- parens (parseField `sepBy` symbol ",")
  returnType <- optional (symbol "->" *> parseType)
  body <- braces (many parseStatement)
  return $ FunctionDeclaration name args returnType body

parseReassignment :: Parser Statement
parseReassignment = do
  name <- variableName
  symbol "="
  expr <- parseExpression
  symbol ";"
  return $ ReasignmentStatement name expr

-- Field parser for function arguments or struct fields
parseField :: Parser Field
parseField = do
  name <- variableName
  symbol ":"
  t <- parseType
  return (name, t)

-- Program parser
parseProgram :: Parser Program
parseProgram = Program <$> many parseStatement

-- Braces helper
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- Main function for testing
main :: IO ()
main = do
  let input = "let x: i32 = 42;\nfn add(a: i32, b: i32) -> i32 {\n  let result: i32 = a + b;\n  result\n}\n"
  case runParser parseProgram "<stdin>" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right program -> print program
