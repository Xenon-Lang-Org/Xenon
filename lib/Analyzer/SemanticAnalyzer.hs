{-
-- EPITECH PROJECT, 2025
-- Xenon [WSL: Ubuntu]
-- File description:
-- SemanticAnalyzer
-}

module Analyzer.SemanticAnalyzer
  ( analyze
  , AnalysisError(..)
  , AnalysisResult(..)
  , AnalysisContext(..)
  ) where

import qualified Data.Map as Map
import Parser.Data.Ast
import Analyzer.IR

-------------------------------------------------------------------------------
-- | Analysis context tracks variables, types, and function definitions
--
-------------------------------------------------------------------------------
data AnalysisContext = AnalysisContext 
  { variables :: Map.Map VariableName (Type, Bool)  -- Type and initialization status
  , customTypes :: Map.Map String Type              -- User types
  , functions :: Map.Map FunctionName ([Field], Type) -- Function signatures
  , inLoop :: Bool                                  -- Loop checker
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Possible analysis errors
-------------------------------------------------------------------------------
data AnalysisError
  = UndefinedVariable String
  | UndefinedType String
  | UndefinedFunction String
  | TypeMismatch Type Type
  | MutabilityError String
  | UninitializedVariable String
  | InvalidReturnType Type Type
  | MissingReturnStatement FunctionName
  | InvalidArrayAccess
  | InvalidPointerOperation String
  | CustomError String
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Result of semantic analysis and IR generation
-- |  The final AST to be used for
-------------------------------------------------------------------------------
data AnalysisResult = AnalysisResult 
  { finalAst :: Program
  , irCode :: IR
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Empty analysis context
-------------------------------------------------------------------------------
emptyContext :: AnalysisContext
emptyContext = AnalysisContext 
  { variables = Map.empty
  , customTypes = Map.empty
  , functions = Map.empty
  , inLoop = False
  }

-------------------------------------------------------------------------------
-- | Main entry point for semantic analysis
-- | This function will analyze the program and return either the "finalAst"
-- | and IR code or a list of errors
-------------------------------------------------------------------------------
analyze :: Program -> Either [AnalysisError] AnalysisResult
analyze prog@(Program stmts) = do
  (_, errors) <- analyzeStatements emptyContext stmts
  if null errors
    then do
      let ir = optimizeIR $ astToIR prog
      Right AnalysisResult 
        { finalAst = prog
        , irCode = ir
        }
    else Left errors

-------------------------------------------------------------------------------
-- | Analyze a list of statements
-------------------------------------------------------------------------------
analyzeStatements :: AnalysisContext -> [Statement] -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeStatements ctx [] = Right (ctx, [])
analyzeStatements ctx (stmt:stmts) = do
  (newCtx, errs) <- analyzeStatement ctx stmt
  (finalCtx, moreErrs) <- analyzeStatements newCtx stmts
  Right (finalCtx, errs ++ moreErrs)

-------------------------------------------------------------------------------
-- | Analyze a single statement
-------------------------------------------------------------------------------
analyzeStatement :: AnalysisContext -> Statement -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeStatement ctx stmt = case stmt of
  VariableDeclaration name typ initExpr -> analyzeVarDecl ctx name typ initExpr
  FunctionDeclaration name params retType body -> analyzeFuncDecl ctx name params retType body
  WhileLoop cond body -> analyzeWhile ctx cond body
  If cond thenBody elseBody -> analyzeIf ctx cond thenBody elseBody
  TypeDeclaration name typ -> analyzeTypeDecl ctx name typ
  ReturnStatement expr -> analyzeReturn ctx expr
  StandaloneFunctionCall name args -> analyzeFuncCall ctx name args
  VariableReAssignment name expr -> analyzeVarReassign ctx name expr

-------------------------------------------------------------------------------
-- | Analyze variable declarations
-------------------------------------------------------------------------------
analyzeVarDecl :: AnalysisContext -> VariableName -> Type -> Maybe Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeVarDecl ctx name typ initExpr = 
  case Map.lookup name (variables ctx) of
    Just _ -> Left [MutabilityError $ "Variable " ++ name ++ " already declared"]
    Nothing -> case initExpr of
      Just expr -> do
        exprType <- analyzeExpr ctx expr
        if typeMatches typ exprType
          then Right (ctx { variables = Map.insert name (typ, True) (variables ctx) }, [])
          else Left [TypeMismatch typ exprType]
      Nothing -> Right (ctx { variables = Map.insert name (typ, False) (variables ctx) }, [])

-------------------------------------------------------------------------------
-- | Analyze function declarations
-------------------------------------------------------------------------------
analyzeFuncDecl :: AnalysisContext -> FunctionName -> [Field] -> Type -> Body -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeFuncDecl ctx name params retType body = do
  let funcCtx = ctx { variables = foldr addParam (variables ctx) params }
  (_, bodyErrs) <- analyzeStatements funcCtx body
  if hasValidReturn body retType
    then Right (ctx { functions = Map.insert name (params, retType) (functions ctx) }, bodyErrs)
    else Left [MissingReturnStatement name]
  where
    addParam (named, typ) = Map.insert named (typ, True)

-------------------------------------------------------------------------------
-- | Analyze while loops
-------------------------------------------------------------------------------
analyzeWhile :: AnalysisContext -> Expression -> Body -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeWhile ctx cond body = do
  condType <- analyzeExpr ctx cond
  case condType of
    PrimitiveType _ I32 -> do
      let loopCtx = ctx { inLoop = True }
      (_, bodyErrs) <- analyzeStatements loopCtx body
      Right (ctx, bodyErrs)
    _ -> Left [TypeMismatch (PrimitiveType Immutable I32) condType]

-------------------------------------------------------------------------------
-- | Analyze if statements
-------------------------------------------------------------------------------
analyzeIf :: AnalysisContext -> Expression -> Body -> Maybe Body -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeIf ctx cond thenBody elseBody = do
  condType <- analyzeExpr ctx cond
  case condType of
    PrimitiveType _ I32 -> do
      (_, thenErrs) <- analyzeStatements ctx thenBody
      elseErrs <- case elseBody of
        Just body -> do
          (_, errs) <- analyzeStatements ctx body
          return errs
        Nothing -> return []
      Right (ctx, thenErrs ++ elseErrs)
    _ -> Left [TypeMismatch (PrimitiveType Immutable I32) condType]

-------------------------------------------------------------------------------
-- | Analyze type declarations
-------------------------------------------------------------------------------
analyzeTypeDecl :: AnalysisContext -> String -> Type -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeTypeDecl ctx name typ = 
  case Map.lookup name (customTypes ctx) of
    Just _ -> Left [CustomError $ "Type " ++ name ++ " already declared"]
    Nothing -> Right (ctx { customTypes = Map.insert name typ (customTypes ctx) }, [])

-------------------------------------------------------------------------------
-- | Analyze return statements
-------------------------------------------------------------------------------
analyzeReturn :: AnalysisContext -> Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeReturn ctx expr = do
  _ <- analyzeExpr ctx expr
  Right (ctx, [])

-------------------------------------------------------------------------------
-- | Analyze function calls
-------------------------------------------------------------------------------
analyzeFuncCall :: AnalysisContext -> FunctionName -> [Expression] -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeFuncCall ctx name args = do
  case Map.lookup name (functions ctx) of
    Nothing -> Left [UndefinedFunction name]
    Just (params, _) -> do
      if length params /= length args
        then Left [CustomError $ "Wrong number of arguments for function " ++ name]
        else do
          argTypes <- mapM (analyzeExpr ctx) args
          let paramTypes = map snd params
          if all (uncurry typeMatches) (zip paramTypes argTypes)
            then Right (ctx, [])
            else Left [TypeMismatch (head paramTypes) (head argTypes)]

-------------------------------------------------------------------------------
-- | Analyze variable reassignment
-- Checks if the variable is defined and if the new value has the same type
-------------------------------------------------------------------------------
analyzeVarReassign :: AnalysisContext -> VariableName -> Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeVarReassign ctx name expr =
  case Map.lookup name (variables ctx) of
    Nothing -> Left [UndefinedVariable name]
    Just (typ, _) -> do
      exprType <- analyzeExpr ctx expr
      if typeMatches typ exprType
        then Right (ctx, [])
        else Left [TypeMismatch typ exprType]

-------------------------------------------------------------------------------
-- | Analyze expressions
-------------------------------------------------------------------------------
analyzeExpr :: AnalysisContext -> Expression -> Either [AnalysisError] Type
analyzeExpr ctx expr = case expr of
  Variable name -> case Map.lookup name (variables ctx) of
    Just (typ, initialized) -> 
      if initialized
        then Right typ
        else Left [UninitializedVariable name]
    Nothing -> Left [UndefinedVariable name]
  
  ELiteral (IntLiteral _) -> Right (PrimitiveType Immutable I32)
  ELiteral (FloatLiteral _) -> Right (PrimitiveType Immutable F32)
  
  BinaryOp _ e1 e2 -> do
    t1 <- analyzeExpr ctx e1
    t2 <- analyzeExpr ctx e2
    if typeMatches t1 t2
      then Right t1
      else Left [TypeMismatch t1 t2]
  
  UnaryOp _ e -> analyzeExpr ctx e
  
  Parenthesis e -> analyzeExpr ctx e
  
  FunctionCall name args -> case Map.lookup name (functions ctx) of
    Nothing -> Left [UndefinedFunction name]
    Just (params, retType) -> do
      argTypes <- mapM (analyzeExpr ctx) args
      let paramTypes = map snd params
      if length args /= length params
        then Left [CustomError $ "Wrong number of arguments for function " ++ name]
        else if all (uncurry typeMatches) (zip paramTypes argTypes)
          then Right retType
          else Left [TypeMismatch (head paramTypes) (head argTypes)]

-------------------------------------------------------------------------------
-- | Helper function to check if types match
-------------------------------------------------------------------------------
typeMatches :: Type -> Type -> Bool
typeMatches (PrimitiveType _ t1) (PrimitiveType _ t2) = t1 == t2
typeMatches (PointerType _ t1) (PointerType _ t2) = typeMatches t1 t2
typeMatches (StructType _ s1) (StructType _ s2) = s1 == s2
typeMatches (ArrayType _ a1) (ArrayType _ a2) = a1 == a2
typeMatches (EnumType _ e1) (EnumType _ e2) = e1 == e2
typeMatches (CustomType _ n1) (CustomType _ n2) = n1 == n2
typeMatches _ _ = False

-------------------------------------------------------------------------------
-- | Helper to check if a function body has a valid return statement
-------------------------------------------------------------------------------
hasValidReturn :: Body -> Type -> Bool
hasValidReturn [] _ = False
hasValidReturn (ReturnStatement _:_) _ = True
hasValidReturn (_:rest) retType = hasValidReturn rest retType