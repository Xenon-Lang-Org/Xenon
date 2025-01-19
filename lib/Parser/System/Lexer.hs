{-# LANGUAGE TypeFamilies #-}

module Parser.System.Lexer
  ( TokenStream (..),
    Token (..),
    TokenType (..),
    tokens,
  )
where

import Data.Foldable (Foldable (toList))
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-------------------------------------------------------------------------------
-- Token and TokenType definitions
-------------------------------------------------------------------------------

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
  | TColon -- ':'
  | TArrow -- '->'
  | TSemicolon -- ';'
  | TComma -- ','
  | TDot -- '.'
  | TOpenParen -- '('
  | TCloseParen -- ')'
  | TOpenBrace -- '{'
  | TCloseBrace -- '}'
  | TOpenBracket -- '['
  | TCloseBracket -- ']'
  -- operators
  | TNot -- '!'
  | TAddressOf -- '?'
  | TDereference -- '@'
  | TNor -- '~'
  --
  | TMult -- '*'
  | TDiv -- '/'
  | TMod -- '%'
  --
  | TPlus -- '+'
  | TMinus -- '-'
  --
  | TEq -- '=='
  | TNotEq -- '!='
  | TLessEq -- '<='
  | TGreaterEq -- '>='
  | TLessThan -- '<'
  | TGreaterThan -- '>'
  --
  | TAnd -- '&&'
  | TOr -- '||'
  --
  | TBitAnd -- '&'
  | TBitOr -- '|'
  | TBitXor -- '^'
  --
  | TLShift -- '<<'
  | TRShift -- '>>'
  --
  | TEqSign -- '='
  | TIntLit !Integer
  | TFloatLit !Double
  | TIdent !String
  | TEOF
  deriving (Eq, Ord)

instance Show TokenType where
  show TLet = "let"
  show TFn = "fn"
  show TIf = "if"
  show TElif = "elif"
  show TElse = "else"
  show TWhile = "while"
  show TReturn = "return"
  show TType = "type"
  show TMut = "mut"
  show TColon = ":"
  show TArrow = "->"
  show TSemicolon = ";"
  show TComma = ","
  show TDot = "."
  show TOpenParen = "("
  show TCloseParen = ")"
  show TOpenBrace = "{"
  show TCloseBrace = "}"
  show TOpenBracket = "["
  show TCloseBracket = "]"
  show TNot = "!"
  show TAddressOf = "?"
  show TDereference = "@"
  show TNor = "~"
  show TMult = "*"
  show TDiv = "/"
  show TMod = "%"
  show TPlus = "+"
  show TMinus = "-"
  show TEq = "=="
  show TNotEq = "!="
  show TLessEq = "<="
  show TGreaterEq = ">="
  show TLessThan = "<"
  show TGreaterThan = ">"
  show TAnd = "&&"
  show TOr = "||"
  show TBitAnd = "&"
  show TBitOr = "|"
  show TBitXor = "^"
  show TLShift = "<<"
  show TRShift = ">>"
  show TEqSign = "="
  show (TIntLit i) = show i
  show (TFloatLit f) = show f
  show (TIdent s) = s
  show TEOF = ""

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
  MP.notFollowedBy (C.alphaNumChar MP.<|> C.char '_')
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

pDot :: Lexer Token
pDot = symbol "." TDot

pEqSign :: Lexer Token
pEqSign = symbol "=" TEqSign

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

pEq :: Lexer Token
pEq = symbol "==" TEq

pNotEq :: Lexer Token
pNotEq = symbol "!=" TNotEq

pLessEq :: Lexer Token
pLessEq = symbol "<=" TLessEq

pGreaterEq :: Lexer Token
pGreaterEq = symbol ">=" TGreaterEq

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
pAnd = symbol "&&" TAnd

pOr :: Lexer Token
pOr = symbol "||" TOr

pBitAnd :: Lexer Token
pBitAnd = symbol "&" TBitAnd

pBitOr :: Lexer Token
pBitOr = symbol "|" TBitOr

pNot :: Lexer Token
pNot = symbol "!" TNot

pLess :: Lexer Token
pLess = symbol "<" TLessThan

pGreater :: Lexer Token
pGreater = symbol ">" TGreaterThan

pXor :: Lexer Token
pXor = symbol "^" TBitXor

pNor :: Lexer Token
pNor = symbol "~" TNor

pLShift :: Lexer Token
pLShift = symbol "<<" TLShift

pRShift :: Lexer Token
pRShift = symbol ">>" TRShift

-- Integer and Float literals
pIntLit :: Lexer Token
pIntLit = lexeme $ do
  pos <- MP.getSourcePos
  num <- L.decimal
  return $ Token pos (TIntLit num)

pFloatLit :: Lexer Token
pFloatLit = lexeme $ do
  pos <- MP.getSourcePos
  f <- L.float
  return $ Token pos (TFloatLit f)

-- Identifiers (must not match reserved keywords)
pIdent :: Lexer Token
pIdent = lexeme $ do
  pos <- MP.getSourcePos
  name <- (:) <$> (C.letterChar MP.<|> C.char '_') <*> MP.many (C.alphaNumChar MP.<|> C.char '_')
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
      pDot,
      pEq,
      pNotEq,
      pLessEq,
      pGreaterEq,
      pLShift,
      pRShift,
      pEqSign,
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
      pMinus,
      pPlus,
      pMult,
      pDiv,
      pMod,
      pAnd,
      pOr,
      pBitAnd,
      pBitOr,
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

-------------------------------------------------------------------------------
-- TokenStream for Megaparsec parser
-------------------------------------------------------------------------------

data TokenStream = TokenStream {getTokenStream :: [Token], originalStream :: String}
  deriving (Show)

instance MP.Stream TokenStream where
  type Token TokenStream = Token
  type Tokens TokenStream = [Token]

  tokenToChunk _ t = [t]
  tokensToChunk _ ts = ts
  chunkToTokens _ ts = ts
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ (TokenStream [] _) = Nothing
  take1_ (TokenStream (t : ts) o) = Just (t, TokenStream ts o)

  takeN_ n (TokenStream s o)
    | n <= 0 = Just ([], TokenStream s o)
    | null s = Nothing
    | otherwise =
        Just (splitAt n s) >>= \(x, y) ->
          Just (x, TokenStream y o)

  takeWhile_ f (TokenStream s o) =
    let (x, y) = span f s
     in (x, TokenStream y o)

instance MP.TraversableStream TokenStream where
  reachOffset targetOffset pst =
    let currentOffset = MP.pstateOffset pst
        needed = targetOffset - currentOffset

        TokenStream ts original = MP.pstateInput pst

        (consumed, remaining) =
          if needed <= 0
            then ([], ts)
            else splitAt needed ts

        newPos :: MP.SourcePos
        newPos =
          case reverse consumed of
            (Token pos _ : _) -> pos
            _ -> MP.pstateSourcePos pst

        lineNum = MP.unPos (MP.sourceLine newPos) - 1
        colNum = MP.unPos (MP.sourceColumn newPos) - 1
        allLines = lines original
        thisLine =
          if lineNum >= 0 && lineNum < length allLines
            then allLines !! lineNum
            else ""

        (prefix, _suffix) = splitAt colNum thisLine

        newPosState =
          pst
            { MP.pstateOffset = targetOffset,
              MP.pstateSourcePos = newPos,
              MP.pstateInput = TokenStream remaining original,
              MP.pstateLinePrefix = prefix,
              MP.pstateTabWidth = MP.pstateTabWidth pst
            }
     in (Just thisLine, newPosState)


instance MP.VisualStream TokenStream where
  showTokens _ ts = unwords . map showToken $ toList ts
    where
      showToken (Token _ t) = "\"" ++ show t ++ "\""
