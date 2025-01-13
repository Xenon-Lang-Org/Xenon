{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.System.Parser
  ( parseProgram,
  )
where

import Control.Monad.Combinators.Expr
import Data.Char (isUpper)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Parser.Data.Ast
import Parser.System.Lexer (Token (..), TokenStream, TokenType (..))
import qualified Parser.System.Lexer as Lexer
import Text.Megaparsec hiding (Token)
import qualified Text.Megaparsec as MP

data ParsingError
  = UnexpectedToken TokenType MP.SourcePos
  | CustomError String
  deriving (Eq, Ord, Show)

instance MP.ShowErrorComponent ParsingError where
  showErrorComponent (UnexpectedToken t pos) =
    "Unexpected token: " ++ show t ++ " at " ++ show pos
  showErrorComponent (CustomError s) = s

type Parser = Parsec ParsingError TokenStream

-------------------------------------------------------------------------------
-- Token Matching Helpers
-------------------------------------------------------------------------------

-- Match a specific token type
matchToken :: (TokenType -> Bool) -> Parser Token
matchToken f = token testToken mempty
  where
    testToken t@(Token _ tp)
      | f tp = Just t
      | otherwise = Nothing

matchTokenType :: TokenType -> Parser Token
matchTokenType ttype = matchToken (== ttype) <?> show ttype

-------------------------------------------------------------------------------
-- Parse Program
-------------------------------------------------------------------------------
parseProgram :: Parser Program
parseProgram = Program <$> many parseStatement

-------------------------------------------------------------------------------
-- Parse Statements
-------------------------------------------------------------------------------
parseStatement :: Parser Statement
parseStatement =
  choice
    [ label "variable declaration" parseVariableDeclaration,
      label "function declaration" parseFunctionDeclaration,
      label "while loop" parseWhileLoop,
      label "if statement" parseIf,
      label "type declaration" parseTypeDeclaration,
      label "return statement" parseReturnStatement,
      -- both parseVariable and parseStandaloneFunctionCall start with an identifier, so we need to try one first
      label "variable reassignment" $ try parseStandaloneFunctionCall,
      label "variable reassignment" parseVariableReAssignment
    ]
    <?> "Line must start with a valid statement"

-------------------------------------------------------------------------------
-- Variable Declaration: let <name>: <type> = <expr>;
-------------------------------------------------------------------------------
parseVariableDeclaration :: Parser Statement
parseVariableDeclaration = do
  _ <- matchTokenType TLet
  name <- parseIdentifier <?> "variable name"
  _ <- matchTokenType TColon
  ty <- parseType <?> "variable type"
  initVal <- optional (matchTokenType TEqSign *> parseExpression)
  _ <- matchTokenType TSemicolon
  return $ VariableDeclaration name ty initVal

-------------------------------------------------------------------------------
-- Function Declaration: fn <name>(<args>) -> <type> { <body> }
-------------------------------------------------------------------------------
parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = do
  _ <- matchTokenType TFn
  name <- parseIdentifier <?> "function name"
  _ <- matchTokenType TOpenParen
  args <- parseField `sepBy` matchTokenType TComma
  _ <- matchTokenType TCloseParen
  _ <- matchTokenType TArrow
  retType <- parseType <?> "return type"
  body <- parseBlock <?> "function body"
  return $ FunctionDeclaration name args retType body

-------------------------------------------------------------------------------
-- While Loop: while <condition> { <body> }
-------------------------------------------------------------------------------
parseWhileLoop :: Parser Statement
parseWhileLoop = do
  _ <- matchTokenType TWhile
  cond <- parseExpression <?> "while loop condition"
  body <- parseBlock <?> "while loop body"
  return $ WhileLoop cond body

-------------------------------------------------------------------------------
-- If: if <cond> { <then_body> } [else { <else_body> }]
-------------------------------------------------------------------------------
parseIf :: Parser Statement
parseIf = do
  _ <- matchTokenType TIf
  cond <- parseExpression <?> "if condition"
  thenBody <- parseBlock <?> "if body"
  elifBodies <- many parseElif
  elseBody <- optional (matchTokenType TElse *> parseBlock <?> "else body")
  return $ transformIf cond thenBody elifBodies elseBody

-------------------------------------------------------------------------------
-- Elif: elif <cond> { <body> }
-------------------------------------------------------------------------------
parseElif :: Parser (Expression, Body)
parseElif = do
  _ <- matchTokenType TElif
  cond <- parseExpression <?> "elif condition"
  body <- parseBlock <?> "elif body"
  return (cond, body)

-- Transform a list of elifs into a nested if-else structure
transformIf :: Expression -> Body -> [(Expression, Body)] -> Maybe Body -> Statement
transformIf cond thenBody elifBodies elseBody =
  case elifBodies of
    [] -> If cond thenBody elseBody
    (elifCond, elifBody) : rest ->
      If cond thenBody (Just [transformIf elifCond elifBody rest elseBody])

-------------------------------------------------------------------------------
-- Type Declaration: type <name> = { ... } | [size: T] | <A, B, C>
-------------------------------------------------------------------------------

parseTypeDeclaration :: Parser Statement
parseTypeDeclaration = do
  _ <- matchTokenType TType
  name <- parseIdentifier <?> "type name"
  _ <- matchTokenType TEqSign
  tyDef <- parseType <?> "type definition"
  _ <- matchTokenType TSemicolon
  return $ TypeDeclaration name tyDef

-------------------------------------------------------------------------------
-- Standalone Function Call: <func_name>(<args>);
-------------------------------------------------------------------------------
parseStandaloneFunctionCall :: Parser Statement
parseStandaloneFunctionCall = do
  name <- parseIdentifier
  _ <- matchTokenType TOpenParen
  args <- parseExpression `sepBy` matchTokenType TComma
  _ <- matchTokenType TCloseParen
  _ <- matchTokenType TSemicolon
  return $ StandaloneFunctionCall name args

-------------------------------------------------------------------------------
-- Variable Reassignment: <name> = <expr>;
-------------------------------------------------------------------------------
parseVariableReAssignment :: Parser Statement
parseVariableReAssignment = do
  name <- parseIdentifier <?> "variable name"
  _ <- matchTokenType TEqSign
  expr <- parseExpression <?> "expression"
  _ <- matchTokenType TSemicolon
  return $ VariableReAssignment name expr

-------------------------------------------------------------------------------
-- Return Statement: return <expr>;
-------------------------------------------------------------------------------
parseReturnStatement :: Parser Statement
parseReturnStatement = do
  _ <- matchTokenType TReturn
  expr <- parseExpression <?> "return expression"
  _ <- matchTokenType TSemicolon
  return $ ReturnStatement expr

-------------------------------------------------------------------------------
-- Parse Fields: <name>: <type>
-------------------------------------------------------------------------------
parseField :: Parser Field
parseField = do
  name <- parseIdentifier <?> "field name"
  _ <- matchTokenType TColon
  ty <- parseType <?> "field type"
  return (name, ty)

-------------------------------------------------------------------------------
-- Parse Expressions
-------------------------------------------------------------------------------
parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm operatorTable

parseTerm :: Parser Expression
parseTerm =
  choice
    [ label "parenthesis expression" parseParenthesis,
      label "literal" parseLiteral,
      label "function call" (try parseFunctionCall),
      label "variable" parseVariable
    ]
    <?> "term"

-------------------------------------------------------------------------------
-- Parse Parenthesis: ( <expr> )
-------------------------------------------------------------------------------
parseParenthesis :: Parser Expression
parseParenthesis = between (matchTokenType TOpenParen) (matchTokenType TCloseParen) parseExpression

-------------------------------------------------------------------------------
-- Parse Literals
-------------------------------------------------------------------------------
parseLiteral :: Parser Expression
parseLiteral = do
  tok <- matchToken isLiteral <?> "literal"
  case tokenType tok of
    TIntLit n -> return $ ELiteral (IntLiteral n)
    TFloatLit f -> return $ ELiteral (FloatLiteral f)
    _ -> fail "Unexpected token, expected literal"
  where
    isLiteral (TIntLit _) = True
    isLiteral (TFloatLit _) = True
    isLiteral _ = False

parseVariable :: Parser Expression
parseVariable = do
  tok <- matchToken isIdent <?> "variable name"
  case tokenType tok of
    TIdent name -> return $ Variable name
    _ -> fail "Unexpected token, expected identifier"
  where
    isIdent (TIdent _) = True
    isIdent _ = False

parseFunctionCall :: Parser Expression
parseFunctionCall = do
  name <- parseIdentifier <?> "function name"
  args <- between (matchTokenType TOpenParen) (matchTokenType TCloseParen) (parseExpression `sepBy` matchTokenType TComma)
  return $ FunctionCall name args

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ Prefix (UnaryOp Negate <$ matchTokenType TNot),
      Prefix (UnaryOp Dereference <$ matchTokenType TDereference),
      Prefix (UnaryOp AddressOf <$ matchTokenType TAddressOf),
      Prefix (UnaryOp BitNot <$ matchTokenType TNor)
      -- Prefix (UnaryOp Negative <$ matchTokenType TMinus) // TODO implement negative operator
    ],
    [ InfixL (BinaryOp Mul <$ matchTokenType TMult),
      InfixL (BinaryOp Div <$ matchTokenType TDiv),
      InfixL (BinaryOp Mod <$ matchTokenType TMod)
    ],
    [ InfixL (BinaryOp Add <$ matchTokenType TPlus),
      InfixL (BinaryOp Sub <$ matchTokenType TMinus)
    ],
    [ InfixN (BinaryOp Eq <$ matchTokenType TEq),
      InfixN (BinaryOp Neq <$ matchTokenType TNotEq),
      InfixN (BinaryOp Lt <$ matchTokenType TLessThan),
      InfixN (BinaryOp Gt <$ matchTokenType TGreaterThan),
      InfixN (BinaryOp Le <$ matchTokenType TLessEq),
      InfixN (BinaryOp Ge <$ matchTokenType TGreaterEq)
    ],
    [ InfixL (BinaryOp And <$ matchTokenType TAnd),
      InfixL (BinaryOp Or <$ matchTokenType TOr)
    ],
    [ InfixL (BinaryOp BitAnd <$ matchTokenType TBitAnd),
      InfixL (BinaryOp BitOr <$ matchTokenType TBitOr),
      InfixL (BinaryOp BitXor <$ matchTokenType TBitXor)
    ],
    [ InfixL (BinaryOp Shl <$ matchTokenType TLShift),
      InfixL (BinaryOp Shr <$ matchTokenType TRShift)
    ]
  ]

-------------------------------------------------------------------------------
-- Parse Type
-------------------------------------------------------------------------------

parseType :: Parser Type
parseType =
  choice
    [ label "primitive type (i32, i64, ...)" $ try parsePrimitiveType,
      label "pointer type (*mut i32)" parsePointerType,
      label "struct type" parseStructType,
      label "array type" parseArrayType,
      label "enum type" parseEnumType,
      label "custom type defined previously" parseCustomType
    ]
    <?> "type"

parseMutablility :: Parser Mutablility
parseMutablility = do
  mut <- optional (matchTokenType TMut)
  return $ if isJust mut then Mutable else Immutable

parsePrimitiveType :: Parser Type
parsePrimitiveType = do
  mutability <- parseMutablility <?> "mutability"
  tok <- matchToken isPrimitive
  case tokenType tok of
    TIdent "i8" -> return $ PrimitiveType mutability I8
    TIdent "i16" -> return $ PrimitiveType mutability I16
    TIdent "i32" -> return $ PrimitiveType mutability I32
    TIdent "i64" -> return $ PrimitiveType mutability I64
    TIdent "u8" -> return $ PrimitiveType mutability U8
    TIdent "u16" -> return $ PrimitiveType mutability U16
    TIdent "u32" -> return $ PrimitiveType mutability U32
    TIdent "u64" -> return $ PrimitiveType mutability U64
    TIdent "f32" -> return $ PrimitiveType mutability F32
    TIdent "f64" -> return $ PrimitiveType mutability F64
    _ -> fail "Invalid type"
  where
    isPrimitive (TIdent "i8") = True
    isPrimitive (TIdent "i16") = True
    isPrimitive (TIdent "i32") = True
    isPrimitive (TIdent "i64") = True
    isPrimitive (TIdent "u8") = True
    isPrimitive (TIdent "u16") = True
    isPrimitive (TIdent "u32") = True
    isPrimitive (TIdent "u64") = True
    isPrimitive (TIdent "f32") = True
    isPrimitive (TIdent "f64") = True
    isPrimitive _ = False

parsePointerType :: Parser Type
parsePointerType = do
  _ <- matchTokenType TMult
  mutability <- parseMutablility <?> "pointer mutability"
  ty <- parseType <?> "pointer type"
  return $ PointerType mutability ty

-------------------------------------------------------------------------------
-- Parse Struct Definition: { <fields> }
-------------------------------------------------------------------------------
parseStructType :: Parser Type
parseStructType = do
  mutability <- parseMutablility <?> "struct mutability"
  _ <- matchTokenType TOpenBrace
  fields <- parseField `sepBy` matchTokenType TComma
  _ <- matchTokenType TCloseBrace
  return $ StructType mutability (Struct fields)

-------------------------------------------------------------------------------
-- Parse Array Type: [<size>: <type>]
-------------------------------------------------------------------------------
parseArrayType :: Parser Type
parseArrayType = do
  mutability <- parseMutablility <?> "array mutability"
  _ <- matchTokenType TOpenBracket
  size <- fromIntegral <$> parseIntLiteral <?> "array size"
  _ <- matchTokenType TColon
  ty <- parseType <?> "array type"
  _ <- matchTokenType TCloseBracket
  return $ ArrayType mutability (Array size ty)

-------------------------------------------------------------------------------
-- Parse Enum Type: <variant1, variant2, ...>
-------------------------------------------------------------------------------
parseEnumType :: Parser Type
parseEnumType = do
  mutability <- parseMutablility <?> "enum mutability"
  _ <- matchTokenType TLessThan
  variants <- parseIdentifier `sepBy` matchTokenType TComma
  _ <- matchTokenType TGreaterThan
  return $ EnumType mutability (EnumT variants)

parseCustomType :: Parser Type
parseCustomType = do
  mutability <- parseMutablility <?> "custom type mutability"
  tok <- matchToken isCustom <?> "custom type name"
  case tokenType tok of
    TIdent name -> return $ CustomType mutability name
    _ -> fail "Custom type names must start with a capital letter"
  where
    isCustom (TIdent (c : _)) = isUpper c
    isCustom _ = False

-------------------------------------------------------------------------------
-- Parse Block
-------------------------------------------------------------------------------
parseBlock :: Parser Body
parseBlock = between (matchTokenType TOpenBrace) (matchTokenType TCloseBrace) (many parseStatement)

-------------------------------------------------------------------------------
-- Parse Identifiers and Literals
-------------------------------------------------------------------------------
parseIdentifier :: Parser String
parseIdentifier = do
  tok <- matchToken isIdent <?> "identifier"
  case tokenType tok of
    TIdent name -> return name
    _ -> fail "Unexpected token, expected identifier"
  where
    isIdent (TIdent _) = True
    isIdent _ = False

parseIntLiteral :: Parser Word64
parseIntLiteral = do
  tok <- matchToken isInt <?> "integer literal"
  case tokenType tok of
    TIntLit n ->
      if n > toInteger (maxBound :: Word64)
        then fail "Integer literal too large"
        else return (fromIntegral n)
    _ -> fail "Unexpected token, expected integer literal"
  where
    isInt (TIntLit _) = True
    isInt _ = False

examplemain :: IO ()
examplemain = do
  let testInputs =
        [ "x = 5;",
          "let x: i32 = 5;",
          "let alexikiwi: Kiwi = 42;",
          "fn add(a: i32, b: i32) -> i32 { return a + b; }",
          "while (x < 10) { x = x + 1; }",
          "fn doif(a: i32) -> i32 { let x : mut i32 = 0; if (a == 0) { x = 1; } else { x = 0; } return x;}",
          "type Point = { x: i32, y: i32 };",
          "type IntArray = [10: i32];",
          "type Color = <Red, Green, Blue>;",
          "type Ptr = *mut i32;"
        ]
  mapM_ runTest testInputs
  where
    runTest input = do
      putStrLn $ "Input: " ++ input
      case runLexer input of
        Left err -> putStrLn $ errorBundlePretty err
        Right tokens -> case runParser parseProgram "PARSER" (Lexer.TokenStream tokens input) of
          Left err -> do
            putStrLn $ "tokens: " ++ show tokens
            putStrLn $ errorBundlePretty err
          Right result -> putStrLn $ "Parsed successfully: " ++ show result
      putStrLn ""
    runLexer = runParser Lexer.tokens "LEXER"
