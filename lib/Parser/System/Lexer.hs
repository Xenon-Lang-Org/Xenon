{-# LANGUAGE TypeFamilies #-}

module Parser.System.Lexer
  ( TokenStream (..),
    Token (..),
    TokenType (..),
    tokens,
  )
where

import Data.Void
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- -----------------------------------------------------------------------------
-- Token and TokenType definitions
-- -----------------------------------------------------------------------------

data Token = Token
  { tokenPos :: !MP.SourcePos,
    tokenType :: !TokenType
  }
  deriving (Eq, Ord, Show)

data TokenType
  = TLet -- 'let'
  | TFn -- 'fn'
  | TIf -- 'if'
  | TElif -- 'elif'
  | TElse -- 'else'
  | TWhile -- 'while'
  | TReturn -- 'return'
  | TType -- 'type'
  | TMut -- 'mut'
  | TDot -- '.'
  | TDereference -- '@'
  | TAddressOf -- '?'
  | TColon -- ':'
  | TArrow -- '->'
  | TSemicolon -- ';'
  | TComma -- ','
  | TEqSign -- '='
  | TOpenParen -- '('
  | TCloseParen -- ')'
  | TOpenBrace -- '{'
  | TCloseBrace -- '}'
  | TOpenBracket -- '['
  | TCloseBracket -- ']'
  | TPlus -- '+'
  | TMinus -- '-'
  | TMult -- '*'
  | TDiv -- '/'
  | TMod -- '%'
  | TAnd -- '&'
  | TOr -- '|'
  | TNot -- '!'
  | TLess -- '<'
  | TGreater -- '>'
  | TXor -- '^'
  | TNor -- '~'
  | TIntLit !Integer
  | TFloatLit !Double
  | TIdent !String
  | TEOF
  deriving (Eq, Ord, Show)

type Lexer = MP.Parsec Void String

-------------------------------------------------------------------------------
-- Basic helpers for parsing tokens
-------------------------------------------------------------------------------

-- Skip spaces, line comments, and block comments
sc :: Lexer ()
sc =
  L.space
    C.space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: String -> TokenType -> Lexer Token
symbol str ttype = lexeme $ do
  pos <- MP.getSourcePos
  _ <- C.string str
  return $ Token pos ttype

-- Parse a keyword but ensure it’s not followed by an identifier character
keyword :: String -> TokenType -> Lexer Token
keyword str ttype = lexeme . MP.try $ do
  pos <- MP.getSourcePos
  _ <- C.string str
  MP.notFollowedBy C.alphaNumChar
  return $ Token pos ttype

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

pLet :: Lexer Token
pLet = keyword "let" TLet

pFn :: Lexer Token
pFn = keyword "fn" TFn

pIf :: Lexer Token
pIf = keyword "if" TIf

pElif :: Lexer Token
pElif = keyword "elif" TElif

pElse :: Lexer Token
pElse = keyword "else" TElse

pWhile :: Lexer Token
pWhile = keyword "while" TWhile

pReturn :: Lexer Token
pReturn = keyword "return" TReturn

pType :: Lexer Token
pType = keyword "type" TType

pMut :: Lexer Token
pMut = keyword "mut" TMut

pColon :: Lexer Token
pColon = symbol ":" TColon

pArrow :: Lexer Token
pArrow = symbol "->" TArrow

pSemicolon :: Lexer Token
pSemicolon = symbol ";" TSemicolon

pComma :: Lexer Token
pComma = symbol "," TComma

pAssign :: Lexer Token
pAssign = symbol "=" TEqSign

pOpenParen :: Lexer Token
pOpenParen = symbol "(" TOpenParen

pCloseParen :: Lexer Token
pCloseParen = symbol ")" TCloseParen

pOpenBrace :: Lexer Token
pOpenBrace = symbol "{" TOpenBrace

pCloseBrace :: Lexer Token
pCloseBrace = symbol "}" TCloseBrace

pOpenBracket :: Lexer Token
pOpenBracket = symbol "[" TOpenBracket

pCloseBracket :: Lexer Token
pCloseBracket = symbol "]" TCloseBracket

pDereference :: Lexer Token
pDereference = symbol "@" TDereference

pAddressOf :: Lexer Token
pAddressOf = symbol "?" TAddressOf

pPlus :: Lexer Token
pPlus = symbol "+" TPlus

pMinus :: Lexer Token
pMinus = symbol "-" TMinus

pMult :: Lexer Token
pMult = symbol "*" TMult

pDiv :: Lexer Token
pDiv = symbol "/" TDiv

pMod :: Lexer Token
pMod = symbol "%" TMod

pAnd :: Lexer Token
pAnd = symbol "&" TAnd

pOr :: Lexer Token
pOr = symbol "|" TOr

pNot :: Lexer Token
pNot = symbol "!" TNot

pLess :: Lexer Token
pLess = symbol "<" TLess

pGreater :: Lexer Token
pGreater = symbol ">" TGreater

pXor :: Lexer Token
pXor = symbol "^" TXor

pNor :: Lexer Token
pNor = symbol "~" TNor

-- Integer and Float literals
pIntLit :: Lexer Token
pIntLit = lexeme $ do
  pos <- MP.getSourcePos
  num <- L.signed sc L.decimal
  return $ Token pos (TIntLit num)

pFloatLit :: Lexer Token
pFloatLit = lexeme $ do
  pos <- MP.getSourcePos
  f <- L.signed sc L.float
  return $ Token pos (TFloatLit f)

-- Identifiers (must not match reserved keywords)
pIdent :: Lexer Token
pIdent = lexeme $ do
  pos <- MP.getSourcePos
  name <- (:) <$> C.letterChar <*> MP.many (C.alphaNumChar MP.<|> C.char '_')
  return $ Token pos (TIdent name)

pEOF :: Lexer Token
pEOF = do
  pos <- MP.getSourcePos
  _ <- MP.eof
  return $ Token pos TEOF

-------------------------------------------------------------------------------
-- Combining token parsers
-------------------------------------------------------------------------------

tokenParser :: Lexer Token
tokenParser =
  MP.choice
    [ pLet,
      pFn,
      pIf,
      pElif,
      pElse,
      pWhile,
      pReturn,
      pType,
      pMut,
      pColon,
      pArrow,
      pSemicolon,
      pComma,
      pAssign,
      pOpenParen,
      pCloseParen,
      pOpenBrace,
      pCloseBrace,
      pOpenBracket,
      pCloseBracket,
      pDereference,
      pAddressOf,
      pIdent,
      MP.try pFloatLit, -- try float before int
      pPlus,
      pMinus,
      pMult,
      pDiv,
      pMod,
      pAnd,
      pOr,
      pNot,
      pLess,
      pGreater,
      pXor,
      pNor,
      pIntLit
    ]

tokens :: Lexer [Token]
tokens = do
  sc -- Skip initial whitespace/comments
  ts <- MP.many tokenParser
  eofToken <- pEOF
  return (ts ++ [eofToken])

-- -----------------------------------------------------------------------------
-- TokenStream for Megaparsec parser
-- -----------------------------------------------------------------------------

newtype TokenStream = TokenStream {getTokenStream :: [Token]}
  deriving (Show)

instance MP.Stream TokenStream where
  type Token TokenStream = Token
  type Tokens TokenStream = [Token]

  tokenToChunk _ t = [t]
  tokensToChunk _ ts = ts
  chunkToTokens _ ts = ts
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ (TokenStream []) = Nothing
  take1_ (TokenStream (t : ts)) = Just (t, TokenStream ts)

  takeN_ n (TokenStream s)
    | n <= 0 = Just ([], TokenStream s)
    | null s = Nothing
    | otherwise =
        Just (splitAt n s) >>= \(x, y) ->
          Just (x, TokenStream y)

  takeWhile_ f (TokenStream s) =
    let (x, y) = span f s
     in (x, TokenStream y)
