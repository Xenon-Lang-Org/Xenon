module Parser.System.Parser where

import Control.Monad.Combinators.Expr
import Data.Void
import Parser.Data.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- reserved keywords (return, i32, u32, ect...)
reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

parseInt :: Parser Literal
parseInt = IntLiteral <$> lexeme L.decimal

parseFloat :: Parser Literal
parseFloat = FloatLiteral <$> lexeme L.float

parseLiteral :: Parser Literal
parseLiteral = try parseFloat <|> parseInt

variableName :: Parser String
variableName =
  lexeme $
    string "_" <|> ((:) <$> lowerChar <*> many (alphaNumChar <|> char '_'))

functionName :: Parser String
functionName = lexeme ((:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_'))

parseType :: Parser Type
parseType = (PrimitiveType <$> parsePrimitive) <|> (CustomType <$> parseCustomType)
  where
    parsePrimitive =
      choice
        [ I8 <$ reserved "i8",
          I16 <$ reserved "i16",
          I32 <$ reserved "i32",
          I64 <$ reserved "i64",
          U8 <$ reserved "u8",
          U16 <$ reserved "u16",
          U32 <$ reserved "u32",
          U64 <$ reserved "u64",
          F32 <$ reserved "f32",
          F64 <$ reserved "f64"
        ]
    parseCustomType = lexeme ((:) <$> upperChar <*> many alphaNumChar)

parseExpression :: Parser Expression
parseExpression = makeExprParser term operators
  where
    term =
      choice
        [ Variable <$> variableName,
          ELiteral <$> parseLiteral,
          parens parseExpression
        ]
    operators =
      [ [Prefix (UnaryOp Negate <$ symbol "!")],
        [ InfixL (BinaryOp Mul <$ symbol "*"),
          InfixL (BinaryOp Div <$ symbol "/")
        ],
        [ InfixL (BinaryOp Add <$ symbol "+"),
          InfixL (BinaryOp Sub <$ symbol "-")
        ],
        [ InfixN (BinaryOp Eq <$ symbol "=="),
          InfixN (BinaryOp Neq <$ symbol "!="),
          InfixN (BinaryOp Lt <$ symbol "<"),
          InfixN (BinaryOp Gt <$ symbol ">"),
          InfixN (BinaryOp Le <$ symbol "<="),
          InfixN (BinaryOp Ge <$ symbol ">=")
        ],
        [ InfixL (BinaryOp And <$ symbol "&&"),
          InfixL (BinaryOp Or <$ symbol "||")
        ]
      ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseStatement :: Parser Statement
parseStatement =
  choice
    [ try parseVariableDeclaration,
      try parseFunctionDeclaration,
      try parseReturnStatement,
      ExpressionStatement <$> parseExpression
    ]

parseVariableDeclaration :: Parser Statement
parseVariableDeclaration =
  -- withRecovery recover $
  do
    reserved "let"
    name <- variableName
    _ <- symbol ":"
    t <- parseType
    initializer <- optional (symbol "=" *> parseExpression)
    _ <- symbol ";"
    return $ VariableDeclaration name t initializer

parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration =
  -- withRecovery recover $
  do
    reserved "fn"
    name <- functionName
    args <- parens (parseField `sepBy` symbol ",") <?> "function arguments"
    _ <- symbol "->" <?> "'->' before return type"
    returnType <- parseType <?> "function return type"
    body <- braces (many parseStatement <?> "statements inside function body") <?> "function body"
    return $ FunctionDeclaration name args returnType body

  
parseReturnStatement :: Parser Statement
parseReturnStatement =
  -- withRecovery recover $
  ReturnStatement <$> (reserved "return" *> parseExpression <* symbol ";")

parseField :: Parser Field
parseField =
  (\name _ t -> (name, t))
    <$> variableName
    <*> (symbol ":" <?> "':' after field name")
    <*> parseType

parseProgram :: Parser Program
parseProgram = Program <$> many parseStatement

-- WORK IN PROGRESS
-- recover :: ParseError String Void -> Parser Statement
-- recover err = do
--   registerParseError err
--   skipManyTill anySingle (try $ lookAhead parseStatement <|> eof)
--   return $ ExpressionStatement (Variable "<error>")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

someMain :: IO ()
someMain = do
  let input = "let x: i32 = 42;\nfn add(a: i32, b: i32) - i32 {\n  let result: i32 = a + b;\n  return result;\n}\n"
  case runParser parseProgram "<stdin>" input of
    Left err -> putStrLn $ errorBundlePretty err
    Right result -> putStrLn $ "Parsed successfully: " ++ show result
