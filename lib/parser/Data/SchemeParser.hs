--- To enable parsing using Data.Text instead of String
{-# LANGUAGE OverloadedStrings #-}
-- suggestion from hlint
{-# LANGUAGE StrictData #-}

module Data.SchemeParser
  ( Expr (..),
    parseExpr,
    expr,
    sc,
    boolean,
    call,
    define,
    ifExpr,
    lambda,
    list,
    number,
    symbolExpr,
  )
where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Number Integer
  | Bool Bool
  | Symbol String
  | List [Expr]
  | Define String Expr
  | Lambda [String] Expr
  | If Expr Expr Expr
  | Call Expr [Expr]
  deriving (Show, Eq)

-- | Parser type alias
-- | `Void` is used for the error component could be changed in the future
-- | `Text` is the input stream type
type Parser = Parsec Void Text

-- | space consumer (skip whitespace and comments)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | paretheses parser, parse the content between the parens
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | special characters supported in indentifiers
specialChars :: String
specialChars = "?!+-*/<=>:$%^&_~@"

-- | identifier parser
-- | reference: https://www.scheme.com/tspl4/intro.html#./intro:h1 (second paragraph)
identifier :: Parser String
identifier = lexeme $ do
  first <-
    try (T.unpack <$> chunk "...")
      <|> try (T.unpack <$> chunk "->")
      <|> try (T.unpack <$> chunk "+")
      <|> try (T.unpack <$> chunk "-")
      <|> fmap (: []) (letterChar <|> oneOf (filter (`notElem` ("+-.@0123456789" :: String)) specialChars))
  case first of
    "..." ->
      notFollowedBy (satisfy (not . isSpace)) >> return "..."
    "->" -> do
      rest <- many (alphaNumChar <|> oneOf specialChars)
      return ("->" ++ rest)
    "+" -> notFollowedBy alphaNumChar >> return "+"
    "-" -> notFollowedBy alphaNumChar >> return "-"
    x -> do
      rest <- many (alphaNumChar <|> oneOf specialChars)
      return (x ++ rest)

-- | helper function to parse space speparated parsers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | number parser
number :: Parser Expr
number = lexeme $ do
  sign <- optional (char '-')
  num <- L.decimal
  return $ Number $ case sign of
    Just _ -> -num
    Nothing -> num

-- | bool parser
boolean :: Parser Expr
boolean = lexeme $ (Bool True <$ string "#t") <|> (Bool False <$ string "#f")

-- | symbol expressions parsing
symbolExpr :: Parser Expr
symbolExpr = Symbol <$> identifier

-- | `list` expressions parsing
list :: Parser Expr
list = List <$> parens (many expr)

-- |
-- define expression with pattern "(define (name args) body)"
-- where `body` can be any expression
--
-- Examples:
-- (define (add x y) (+ x y))
-- (define (f x) (if (> x 0) x (- x)))
-- (define (true) #t)
defineExpr :: Parser Expr
defineExpr = do
  (name, args) <- parens $ do
    name <- identifier
    args <- many identifier
    return (name, args)
  body <- expr
  return $ Define name (Lambda args body)

-- |
-- define expression with pattern "(define name value)"
-- where `value` can be any expressiwon
--
-- Examples:
-- (define x 42)
-- (define add (lambda (x y) (+ x y)))
-- (define yes (lambda () #t))
defineValue :: Parser Expr
defineValue = do
  name <- identifier
  value <- expr
  return $ Define name value

-- | `define` expressions parsing
define :: Parser Expr
define = parens $ do
  _ <- symbol "define"
  choice
    [ defineExpr,
      defineValue
    ]

-- | `lambda` expressions parsing
lambda :: Parser Expr
lambda = parens $ do
  _ <- symbol "lambda"
  params <- parens (many identifier)
  body <- expr
  return $ Lambda params body

-- | `if` expressions parsing
ifExpr :: Parser Expr
ifExpr = parens $ do
  _ <- symbol "if"
  cond <- expr
  thenExpr <- expr
  elseExpr <- expr
  return $ If cond thenExpr elseExpr

-- | Function calls
call :: Parser Expr
call = parens $ do
  func <- symbolExpr
  args <- many expr
  return $ Call func args

expr :: Parser Expr
expr =
  sc *> choice
    [ try define,
      try lambda,
      try ifExpr,
      try call,
      try list,
      number,
      boolean,
      symbolExpr
    ]

-- | Parse a Scheme expression
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> expr <* eof) ""
