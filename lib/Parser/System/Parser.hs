{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.System.Parser
  ( parseProgram,
  )
where

import Control.Monad.Combinators.Expr
import Data.Char (isUpper)
import Data.Maybe (isJust)
import Data.Void
import Data.Word (Word64)
import Parser.Data.Ast
import Parser.System.Lexer (Token (..), TokenStream, TokenType (..))
import qualified Parser.System.Lexer as Lexer
import Text.Megaparsec hiding (Token)


type Parser = Parsec Void TokenStream

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
matchTokenType ttype = matchToken (== ttype)

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
    [ parseVariableDeclaration,
      parseFunctionDeclaration,
      parseWhileLoop,
      parseIf,
      parseTypeDeclaration,
      parseReturnStatement,
      -- both parseVariable and parseStandaloneFunctionCall start with an identifier, so we need to try one first
      try parseStandaloneFunctionCall,
      parseVariableReAssignment
    ]

-------------------------------------------------------------------------------
-- Variable Declaration: let <name>: <type> = <expr>;
-------------------------------------------------------------------------------
parseVariableDeclaration :: Parser Statement
parseVariableDeclaration = do
  _ <- matchTokenType TLet
  name <- parseIdentifier
  _ <- matchTokenType TColon
  ty <- parseType
  initVal <- optional (matchTokenType TEqSign *> parseExpression)
  _ <- matchTokenType TSemicolon
  return $ VariableDeclaration name ty initVal

-------------------------------------------------------------------------------
-- Function Declaration: fn <name>(<args>) -> <type> { <body> }
-------------------------------------------------------------------------------
parseFunctionDeclaration :: Parser Statement
parseFunctionDeclaration = do
  _ <- matchTokenType TFn
  name <- parseIdentifier
  _ <- matchTokenType TOpenParen
  args <- parseField `sepBy` matchTokenType TComma
  _ <- matchTokenType TCloseParen
  _ <- matchTokenType TArrow
  retType <- parseType
  body <- parseBlock
  return $ FunctionDeclaration name args retType body

-------------------------------------------------------------------------------
-- While Loop: while <condition> { <body> }
-------------------------------------------------------------------------------
parseWhileLoop :: Parser Statement
parseWhileLoop = do
  _ <- matchTokenType TWhile
  cond <- parseExpression
  body <- parseBlock
  return $ WhileLoop cond body

-------------------------------------------------------------------------------
-- If: if <cond> { <then_body> } [else { <else_body> }]
-------------------------------------------------------------------------------
parseIf :: Parser Statement
parseIf = do
  _ <- matchTokenType TIf
  cond <- parseExpression
  thenBody <- parseBlock
  elifBodies <- many parseElif -- there can be no elifs
  elseBody <- optional (matchTokenType TElse *> parseBlock)
  return $ transformIf cond thenBody elifBodies elseBody

-------------------------------------------------------------------------------
-- Elif: elif <cond> { <body> }
-------------------------------------------------------------------------------
parseElif :: Parser (Expression, Body)
parseElif = do
  _ <- matchTokenType TElif
  cond <- parseExpression
  body <- parseBlock
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
  name <- parseIdentifier
  _ <- matchTokenType TEqSign
  tyDef <- parseTypeDefinition name
  _ <- matchTokenType TSemicolon
  return $ TypeDeclaration tyDef

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
  name <- parseIdentifier
  _ <- matchTokenType TEqSign
  expr <- parseExpression
  _ <- matchTokenType TSemicolon
  return $ VariableReAssignment name expr

-------------------------------------------------------------------------------
-- Parse Type Definitions
-------------------------------------------------------------------------------
parseTypeDefinition :: String -> Parser TypeDefinition
parseTypeDefinition name =
  choice
    [ parseStructDefinition name,
      parseArrayDefinition name,
      parseEnumDefinition name,
      parseAliasDefinition name
    ]

-------------------------------------------------------------------------------
-- Parse Struct Definition: { <fields> }
-------------------------------------------------------------------------------
parseStructDefinition :: String -> Parser TypeDefinition
parseStructDefinition name = do
  _ <- matchTokenType TOpenBrace
  fields <- parseField `sepBy` matchTokenType TComma
  _ <- matchTokenType TCloseBrace
  return $ StructDeclaration name (Struct fields)

-------------------------------------------------------------------------------
-- Parse Array Definition: [<size>: <type>]
-------------------------------------------------------------------------------
parseArrayDefinition :: String -> Parser TypeDefinition
parseArrayDefinition name = do
  _ <- matchTokenType TOpenBracket
  size <- fromIntegral <$> parseIntLiteral
  _ <- matchTokenType TColon
  ty <- parseType
  _ <- matchTokenType TCloseBracket
  return $ ArrayDeclaration name (size, ty)

-------------------------------------------------------------------------------
-- Parse Enum Definition: <variant1, variant2, ...>
-------------------------------------------------------------------------------
parseEnumDefinition :: String -> Parser TypeDefinition
parseEnumDefinition name = do
  _ <- matchTokenType TLessThan
  variants <- parseIdentifier `sepBy` matchTokenType TComma
  _ <- matchTokenType TGreaterThan
  return $ EnumDeclaration name variants

-------------------------------------------------------------------------------
-- Parse Alias Definition: <type>
-------------------------------------------------------------------------------

parseAliasDefinition :: String -> Parser TypeDefinition
parseAliasDefinition name = do
  ty <- parseType
  return $ AliasDeclaration name ty

-------------------------------------------------------------------------------
-- Return Statement: return <expr>;
-------------------------------------------------------------------------------
parseReturnStatement :: Parser Statement
parseReturnStatement = do
  _ <- matchTokenType TReturn
  expr <- parseExpression
  _ <- matchTokenType TSemicolon
  return $ ReturnStatement expr

-------------------------------------------------------------------------------
-- Parse Fields: <name>: <type>
-------------------------------------------------------------------------------
parseField :: Parser Field
parseField = do
  name <- parseIdentifier
  _ <- matchTokenType TColon
  ty <- parseType
  return (name, ty)

-------------------------------------------------------------------------------
-- Parse Expressions
-------------------------------------------------------------------------------
parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm operatorTable

parseTerm :: Parser Expression
parseTerm =
  choice
    [ parseParenthesis,
      parseLiteral,
      parseVariable
    ]

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
  tok <- matchToken isLiteral
  case tokenType tok of
    TIntLit n -> return $ ELiteral (IntLiteral $ fromIntegral n)
    TFloatLit f -> return $ ELiteral (FloatLiteral f)
    _ -> fail "Unexpected token, expected literal"
  where
    isLiteral (TIntLit _) = True
    isLiteral (TFloatLit _) = True
    isLiteral _ = False

parseVariable :: Parser Expression
parseVariable = do
  tok <- matchToken isIdent
  case tokenType tok of
    TIdent name -> return $ Variable name
    _ -> fail "Unexpected token, expected identifier"
  where
    isIdent (TIdent _) = True
    isIdent _ = False

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ Prefix (UnaryOp Negate <$ matchTokenType TNot),
      Prefix (UnaryOp Dereference <$ matchTokenType TDereference),
      Prefix (UnaryOp AddressOf <$ matchTokenType TAddressOf),
      Prefix (UnaryOp BitNot <$ matchTokenType TNor)
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
    [ parsePrimitiveType,
      parsePointerType,
      parseCustomType
    ]

parsePrimitiveType :: Parser Type
parsePrimitiveType = do
  tok <- matchToken isPrimitive
  case tokenType tok of
    TIdent "i8" -> return $ PrimitiveType I8
    TIdent "i16" -> return $ PrimitiveType I16
    TIdent "i32" -> return $ PrimitiveType I32
    TIdent "i64" -> return $ PrimitiveType I64
    TIdent "u8" -> return $ PrimitiveType U8
    TIdent "u16" -> return $ PrimitiveType U16
    TIdent "u32" -> return $ PrimitiveType U32
    TIdent "u64" -> return $ PrimitiveType U64
    TIdent "f32" -> return $ PrimitiveType F32
    TIdent "f64" -> return $ PrimitiveType F64
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
  mutability <- optional (matchTokenType TMut)
  ty <- parseType
  return $ PointerType (if isJust mutability then Mutable else Immutable) ty

parseCustomType :: Parser Type
parseCustomType = do
  tok <- matchToken isCustom
  case tokenType tok of
    TIdent name -> return $ CustomType name
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
  tok <- matchToken isIdent
  case tokenType tok of
    TIdent name -> return name
    _ -> fail "Unexpected token, expected identifier"
  where
    isIdent (TIdent _) = True
    isIdent _ = False

parseIntLiteral :: Parser Word64
parseIntLiteral = do
  tok <- matchToken isInt
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
          "fn add(a: i32, b: i32) -> i32 { return a + b; }",
          "while (x < 10) { x = x + 1; }",
          "if (x == 0) { x = 1; } else { x = 0; }",
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
        Left err -> putStrLn $ "lexer: " ++ errorBundlePretty err
        Right tokens -> case runParser parseProgram "FILE" (Lexer.TokenStream tokens) of
          Left err -> do
            putStrLn $ "tokens: " ++ show tokens
            putStrLn $ "parser: " ++ errorBundlePretty err
          Right result -> putStrLn $ "Parsed successfully: " ++ show result
      putStrLn ""
    runLexer = runParser Lexer.tokens "<test>"
