module Analyzer.SemanticAnalyzer
  ( analyze
  , AnalysisError(..)
  , AnalysisResult(..)
  , AnalysisContext(..)
  ) where

import qualified Data.Map as Map
import Parser.Data.Ast
import Control.Monad (when)

-------------------------------------------------------------------------------
-- | Core Data Types
-------------------------------------------------------------------------------

data AnalysisContext = AnalysisContext 
  { variables :: Map.Map String (Type, Bool, Bool)  -- (Type, isInitialized, isMutable)
  , types :: Map.Map String Type                    -- User-defined types
  , functions :: Map.Map String ([Field], Type)     -- Function signatures
  , currentFunction :: Maybe (String, Type)         -- Current function being analyzed
  , inLoop :: Bool                                  -- Loop context
  , scopeLevel :: Int                               -- Scope nesting level
  } deriving (Show, Eq)

data AnalysisError
  = UndefinedVariable String
  | UndefinedType String 
  | TypeMismatch Type Type
  | MutabilityError String
  | InvalidPointerOperation String
  | InvalidReturnType Type Type
  | MissingReturnStatement String
  | DuplicateDefinition String
  | UninitializedVariable String
  | InvalidExpression String
  | CustomError String
  | UnreachableCode String
  deriving (Show, Eq)

data BlockStatus 
  = BlockTerminated -- Bloc qui termine toujours (return)
  | BlockOpen      -- Bloc qui peut continuer
  deriving (Show, Eq)

data AnalysisResult = AnalysisResult 
  { finalAst :: Program
  , symbolTable :: AnalysisContext
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Context Management
-------------------------------------------------------------------------------

emptyContext :: AnalysisContext
emptyContext = AnalysisContext 
  { variables = Map.empty
  , types = Map.empty
  , functions = Map.empty
  , currentFunction = Nothing
  , inLoop = False
  , scopeLevel = 0
  }

enterScope :: AnalysisContext -> AnalysisContext
enterScope ctx = ctx { scopeLevel = scopeLevel ctx + 1 }

exitScope :: AnalysisContext -> AnalysisContext
exitScope ctx = ctx { scopeLevel = scopeLevel ctx - 1 }

-------------------------------------------------------------------------------
-- | Main Analysis Functions
-------------------------------------------------------------------------------

analyze :: Program -> Either [AnalysisError] AnalysisResult
analyze prog@(Program stmts) = case analyzeStatements emptyContext stmts of
  Right (newCtx, _, []) -> Right $ AnalysisResult prog newCtx
  Right (_, _, errs) -> Left errs
  Left errs -> Left errs

analyzeStatements :: AnalysisContext -> [Statement] -> Either [AnalysisError] (AnalysisContext, BlockStatus, [AnalysisError])
analyzeStatements ctx [] = Right (ctx, BlockOpen, [])
analyzeStatements ctx (stmt:stmts) = case analyzeStatement ctx stmt of
  Right (ctx1, BlockTerminated, errs1) -> 
    if null stmts 
      then Right (ctx1, BlockTerminated, errs1)
      else Left [UnreachableCode "Code after return statement is unreachable"]
  Right (ctx1, BlockOpen, errs1) -> case analyzeStatements ctx1 stmts of
    Right (ctx2, status, errs2) -> Right (ctx2, status, errs1 ++ errs2)
    Left errs -> Left errs
  Left errs -> Left errs

analyzeStatement :: AnalysisContext -> Statement -> Either [AnalysisError] (AnalysisContext, BlockStatus, [AnalysisError])
analyzeStatement ctx stmt = case stmt of
  ReturnStatement expr -> case analyzeReturn ctx expr of
    Right (ctx', errs) -> Right (ctx', BlockTerminated, errs)
    Left err -> Left err

  If cond thenBody elseBody -> case inferType ctx cond of
    Right (condType, condErrs) ->
      if not (isLogicalType condType)
        then Left [TypeMismatch (PrimitiveType Immutable I32) condType]
        else case analyzeStatements (enterScope ctx) thenBody of
          Right (_, thenStatus, thenErrs) -> case analyzeElse ctx elseBody of
            Right (elseStatus, elseErrs) -> 
              let status = determineIfStatus thenBody elseBody
              in Right (ctx, status, condErrs ++ thenErrs ++ elseErrs)
            Left err -> Left err
          Left err -> Left err
    Left err -> Left err

  WhileLoop cond body -> case inferType ctx cond of
    Right (condType, condErrs) ->
      if not (isLogicalType condType)
        then Left [TypeMismatch (PrimitiveType Immutable I32) condType]
        else case analyzeStatements (enterScope ctx { inLoop = True }) body of
          Right (_, _, bodyErrs) -> Right (exitScope ctx, BlockOpen, condErrs ++ bodyErrs)
          Left err -> Left err
    Left err -> Left err

  VariableDeclaration name typ initExpr -> case analyzeVarDecl ctx name typ initExpr of
    Right (ctx', errs) -> Right (ctx', BlockOpen, errs)
    Left err -> Left err

  FunctionDeclaration name params retType body -> case analyzeFuncDecl ctx name params retType body of
    Right (ctx', errs) -> Right (ctx', BlockOpen, errs)
    Left err -> Left err

  VariableReAssignment name expr -> case analyzeAssignment ctx name expr of
    Right (ctx', errs) -> Right (ctx', BlockOpen, errs)
    Left err -> Left err

  TypeDeclaration name typ -> case analyzeTypeDecl ctx name typ of
    Right (ctx', errs) -> Right (ctx', BlockOpen, errs)
    Left err -> Left err

  StandaloneFunctionCall name args -> case analyzeFunctionCall ctx name args of
    Right (ctx', errs) -> Right (ctx', BlockOpen, errs)
    Left err -> Left err

-- Helper pour déterminer si un bloc if/else termine
determineIfStatus :: [Statement] -> Maybe [Statement] -> BlockStatus
determineIfStatus thenBody elseBody =
  case elseBody of
    Nothing -> hasReturnPath thenBody
    Just elsePart -> 
      case (hasReturnPath thenBody, hasReturnPath elsePart) of
        (BlockTerminated, BlockTerminated) -> BlockTerminated
        _ -> BlockOpen

-- Helper pour vérifier si un chemin se termine toujours par un return
hasReturnPath :: [Statement] -> BlockStatus
hasReturnPath [] = BlockOpen
hasReturnPath (ReturnStatement _:_) = BlockTerminated
hasReturnPath (If _ thenBody (Just elseBody):rest) = 
  case (hasReturnPath thenBody, hasReturnPath elseBody) of
    (BlockTerminated, BlockTerminated) -> BlockTerminated
    _ -> hasReturnPath rest
hasReturnPath (If _ _ Nothing:rest) = hasReturnPath rest
hasReturnPath (WhileLoop _ _:rest) = hasReturnPath rest  -- Un while ne garantit pas la terminaison
hasReturnPath (_:rest) = hasReturnPath rest

analyzeElse :: AnalysisContext -> Maybe [Statement] -> Either [AnalysisError] (BlockStatus, [AnalysisError])
analyzeElse _ Nothing = Right (BlockOpen, [])
analyzeElse ctx (Just body) = case analyzeStatements (enterScope ctx) body of
  Right (_, status, errs) -> Right (status, errs)
  Left err -> Left err

[... Reste du code inchangé jusqu'à analyzeTypeDecl ...]