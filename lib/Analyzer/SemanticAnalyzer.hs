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
  = BlockTerminated
  | BlockOpen
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

  If cond thenBody elseBody -> analyzeIf ctx cond thenBody elseBody

  WhileLoop cond body -> analyzeWhile ctx cond body 

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


-------------------------------------------------------------------------------
-- | Analysis Implementation
-------------------------------------------------------------------------------

analyzeVarDecl :: AnalysisContext -> String -> Type -> Maybe Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeVarDecl ctx name typ initExpr = case validateType ctx typ of
  Left err -> Left err
  Right validatedType -> 
    if Map.member name (variables ctx)
      then Left [DuplicateDefinition $ "Variable " ++ name ++ " already declared"]
      else let isMutable = case validatedType of
                PrimitiveType Mutable _ -> True
                PointerType Mutable _ -> True
                _ -> False
           in case initExpr of
                Just expr -> case inferType ctx expr of
                  Right (exprType, exprErrs) ->
                    if typeMatches validatedType exprType
                      then Right (ctx { variables = Map.insert name (validatedType, True, isMutable) (variables ctx) }, exprErrs)
                      else Left [TypeMismatch validatedType exprType]
                  Left err -> Left err
                Nothing -> Right (ctx { variables = Map.insert name (validatedType, False, isMutable) (variables ctx) }, [])

analyzeFuncDecl :: AnalysisContext -> String -> [Field] -> Type -> [Statement] 
                -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeFuncDecl ctx name params retType body =
  if Map.member name (functions ctx)
    then Left [DuplicateDefinition $ "Function " ++ name ++ " already declared"]
    else case validateType ctx retType of
      Left err -> Left err
      Right validatedRetType -> case validateParams ctx params of
        Left err -> Left err
        Right validatedParams -> 
          let funcCtx = ctx { 
                currentFunction = Just (name, validatedRetType),
                functions = Map.insert name (validatedParams, validatedRetType) (functions ctx),
                variables = foldr (\(pName, pType) vars -> 
                  let isMutable = case pType of
                        PrimitiveType Mutable _ -> True
                        PointerType Mutable _ -> True
                        _ -> False
                  in Map.insert pName (pType, True, isMutable) vars)
                  (variables ctx) 
                  validatedParams
              }
          in case analyzeStatements (enterScope funcCtx) body of
            Right (_, _, bodyErrs) -> 
              if validatedRetType /= PrimitiveType Immutable I32
                then Right (ctx { functions = Map.insert name (validatedParams, validatedRetType) (functions ctx) }, bodyErrs)
                else case hasReturnStatement body of
                       BlockTerminated -> Right (ctx { functions = Map.insert name (validatedParams, validatedRetType) (functions ctx) }, bodyErrs)
                       BlockOpen -> Left [MissingReturnStatement name]
            Left err -> Left err

validateParams :: AnalysisContext -> [Field] -> Either [AnalysisError] [Field]
validateParams ctx = mapM validateField
  where validateField (name, typ) = case validateType ctx typ of
          Left err -> Left err
          Right validType -> Right (name, validType)

-------------------------------------------------------------------------------
-- |check if a function has a return statement
  -- If without else doesn't guarantee return
  -- While loop doesn't guarantee return
-------------------------------------------------------------------------------

-- hasReturnStatement with block 
hasReturnStatement :: [Statement] -> BlockStatus
hasReturnStatement [] = BlockOpen
hasReturnStatement (ReturnStatement _:_) = BlockTerminated
hasReturnStatement (If _ thenBody (Just elseBody):rest) = 
  case (hasReturnStatement thenBody, hasReturnStatement elseBody) of
    (BlockTerminated, BlockTerminated) -> BlockTerminated
    _ -> hasReturnStatement rest
hasReturnStatement (If _ _ Nothing:rest) = 
  hasReturnStatement rest
hasReturnStatement (WhileLoop _ _:rest) = hasReturnStatement rest
hasReturnStatement (_:rest) = hasReturnStatement rest

analyzeWhile :: AnalysisContext -> Expression -> [Statement] 
             -> Either [AnalysisError] (AnalysisContext, BlockStatus, [AnalysisError])
analyzeWhile ctx cond body = case inferType ctx cond of
  Right (condType, condErrs) ->
    if not (isLogicalType condType)
      then Left [TypeMismatch (PrimitiveType Immutable I32) condType]
      else case analyzeStatements (enterScope ctx { inLoop = True }) body of
        Right (_, _, bodyErrs) -> Right (exitScope ctx, BlockOpen, condErrs ++ bodyErrs)
        Left err -> Left err
  Left err -> Left err

analyzeIf :: AnalysisContext -> Expression -> [Statement] -> Maybe [Statement]
          -> Either [AnalysisError] (AnalysisContext, BlockStatus, [AnalysisError])
analyzeIf ctx cond thenBody elseBody = case inferType ctx cond of
  Right (condType, condErrs) ->
    if not (isLogicalType condType)
      then Left [TypeMismatch (PrimitiveType Immutable I32) condType]
      else let thenCtx = enterScope ctx
            in case analyzeStatements thenCtx thenBody of
              Right (_, thenStatus, thenErrs) -> case analyzeElse ctx elseBody of
                Right (elseStatus, elseErrs) -> 
                  let status = case (thenStatus, elseStatus) of
                        (BlockTerminated, BlockTerminated) -> BlockTerminated
                        _ -> BlockOpen
                  in Right (ctx, status, condErrs ++ thenErrs ++ elseErrs)
                Left err -> Left err
              Left err -> Left err
  Left err -> Left err

analyzeElse :: AnalysisContext -> Maybe [Statement] -> Either [AnalysisError] (BlockStatus, [AnalysisError])
analyzeElse _ Nothing = Right (BlockOpen, [])
analyzeElse ctx (Just body) = case analyzeStatements (enterScope ctx) body of
  Right (_, status, errs) -> Right (status, errs)
  Left err -> Left err

analyzeReturn :: AnalysisContext -> Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeReturn ctx expr = case currentFunction ctx of
  Nothing -> Left [CustomError "Return statement outside function"]
  Just (_, expectedType) -> case inferType ctx expr of
    Right (actualType, errs) ->
      if typeMatches expectedType actualType
        then Right (ctx, errs)
        else Left [InvalidReturnType expectedType actualType]
    Left err -> Left err

analyzeAssignment :: AnalysisContext -> String -> Expression -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeAssignment ctx name expr = 
  case Map.lookup name (variables ctx) of
    Just (varType, _, isMut) -> checkAndAssign ctx name varType isMut expr
    Nothing -> Left [UndefinedVariable name]
  where
    checkAndAssign ctx' name' varType isMut expr' = 
      case varType of
        PrimitiveType Immutable _ -> Left [MutabilityError $ name' ++ " is immutable (missing 'mut' qualifier)"]
        PointerType Immutable _ -> Left [MutabilityError $ name' ++ " is immutable (missing 'mut' qualifier)"]
        _ | not isMut -> Left [MutabilityError $ name' ++ " was not declared as mutable"]
        _ -> case inferType ctx' expr' of
              Right (exprType, exprErrs) ->
                if typeMatches varType exprType
                  then Right (ctx' { variables = Map.insert name' (varType, True, isMut) (variables ctx') }, exprErrs)
                  else Left [TypeMismatch varType exprType]
              Left err -> Left err

analyzeFunctionCall :: AnalysisContext -> String -> [Expression] -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeFunctionCall ctx name args = case Map.lookup name (functions ctx) of
  Nothing -> Left [UndefinedVariable $ "Function " ++ name ++ " not found"]
  Just (params, _) ->
    if length args /= length params
      then Left [CustomError $ "Function " ++ name ++ 
                " expects " ++ show (length params) ++ 
                " arguments but got " ++ show (length args)]
      else case validateArgs ctx args params of
        Right errs -> Right (ctx, concat errs)
        Left err -> Left err

validateArgs :: AnalysisContext -> [Expression] -> [Field] -> Either [AnalysisError] [[AnalysisError]]
validateArgs ctx args params = mapM validateArg (zip args params)
  where validateArg (arg, (_, expectedType)) = case inferType ctx arg of
          Right (actualType, errs) ->
            if typeMatches expectedType actualType
              then Right errs
              else Left [TypeMismatch expectedType actualType]
          Left err -> Left err

inferType :: AnalysisContext -> Expression -> Either [AnalysisError] (Type, [AnalysisError])
inferType ctx expr = case expr of
  Variable name -> case Map.lookup name (variables ctx) of
    Just (typ, isInit, _) ->
      if isInit
        then Right (typ, [])
        else Left [UninitializedVariable name]
    Nothing -> Left [UndefinedVariable name]
      
  ELiteral lit -> case currentFunction ctx of
    Just (_, expectedType) ->
      Right (literalType expectedType lit, [])
    Nothing ->
      Right (literalType (PrimitiveType Immutable I32) lit, [])
      
  BinaryOp op e1 e2 -> case inferType ctx e1 of
    Right (t1, errs1) -> case inferType ctx e2 of
      Right (t2, errs2) ->
        if compatibleTypes t1 t2
          then Right (resultType op t1 t2, errs1 ++ errs2)
          else Left [TypeMismatch t1 t2]
      Left err -> Left err
    Left err -> Left err
      
  UnaryOp op e -> case inferType ctx e of
    Right (t, errs) -> Right (unaryResultType op t, errs)
    Left err -> Left err
    
  FunctionCall name args -> case Map.lookup name (functions ctx) of
    Just (params, retType) ->
      if length args /= length params
        then Left [CustomError $ "Wrong number of arguments for " ++ name]
        else case validateArgs ctx args params of
          Right errs -> Right (retType, concat errs)
          Left err -> Left err
    Nothing -> Left [UndefinedVariable $ "Function " ++ name]
      
  Parenthesis e -> inferType ctx e

-------------------------------------------------------------------------------
-- | Type System Functions
-------------------------------------------------------------------------------

validateType :: AnalysisContext -> Type -> Either [AnalysisError] Type
validateType ctx typ = case typ of
  PointerType mut innerType -> case validateType ctx innerType of
    Right validatedInner -> Right $ PointerType mut validatedInner
    Left err -> Left err
    
  CustomType _ name -> case Map.lookup name (types ctx) of
    Just definedType -> Right definedType
    Nothing -> Left [UndefinedType name]
      
  _ -> Right typ  -- Primitive types are always valid

typeMatches :: Type -> Type -> Bool
typeMatches (PrimitiveType _ t1) (PrimitiveType _ t2) = t1 == t2
typeMatches (PointerType _ t1) (PointerType _ t2) = typeMatches t1 t2
typeMatches (CustomType _ n1) (CustomType _ n2) = n1 == n2
typeMatches _ _ = False

compatibleTypes :: Type -> Type -> Bool
compatibleTypes = typeMatches

-------------------------------------------------------------------------------
-- | Helper Functions
-------------------------------------------------------------------------------

resultType :: BinOp -> Type -> Type -> Type
resultType Add t1 t2 | typeMatches t1 t2 && isNumericType t1 = t1
resultType Sub t1 t2 | typeMatches t1 t2 && isNumericType t1 = t1
resultType Mul t1 t2 | typeMatches t1 t2 && isNumericType t1 = t1
resultType Div t1 t2 | typeMatches t1 t2 && isNumericType t1 = t1
resultType Mod t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
resultType BitAnd t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
                       | otherwise = error "Bitwise operations are only allowed on integer types"
resultType BitOr t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
                      | otherwise = error "Bitwise operations are only allowed on integer types"
resultType BitXor t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
                       | otherwise = error "Bitwise operations are only allowed on integer types"
resultType Shl t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
                     | otherwise = error "Shift operations are only allowed on integer types"
resultType Shr t1 t2 | typeMatches t1 t2 && isIntegerType t1 = t1
                     | otherwise = error "Shift operations are only allowed on integer types"
resultType Eq t1 t2 | typeMatches t1 t2 = PrimitiveType Immutable I32
resultType Neq t1 t2 | typeMatches t1 t2 = PrimitiveType Immutable I32
resultType Lt t1 t2 | typeMatches t1 t2 && isNumericType t1 = PrimitiveType Immutable I32
resultType Gt t1 t2 | typeMatches t1 t2 && isNumericType t1 = PrimitiveType Immutable I32
resultType Le t1 t2 | typeMatches t1 t2 && isNumericType t1 = PrimitiveType Immutable I32
resultType Ge t1 t2 | typeMatches t1 t2 && isNumericType t1 = PrimitiveType Immutable I32
resultType And t1 t2 | isLogicalType t1 && isLogicalType t2 = PrimitiveType Immutable I32
resultType Or t1 t2 | isLogicalType t1 && isLogicalType t2 = PrimitiveType Immutable I32
resultType op t1 t2 = error $ "Invalid operation " ++ show op ++ " between types " ++ show t1 ++ " and " ++ show t2

unaryResultType :: UnaryOp -> Type -> Type
unaryResultType Negate t | isLogicalType t = PrimitiveType Immutable I32
unaryResultType Negative t | isNumericType t = t
unaryResultType BitNot (PrimitiveType mut I32) = PrimitiveType mut I32
unaryResultType BitNot (PrimitiveType mut I64) = PrimitiveType mut I64
unaryResultType BitNot (PrimitiveType mut t) 
  | isIntegerType (PrimitiveType mut t) = PrimitiveType mut I32
unaryResultType Dereference (PointerType _ t) = t
unaryResultType AddressOf t = PointerType Immutable t
unaryResultType _ t = t

literalType :: Type -> Literal -> Type
literalType expectedType lit = case (expectedType, lit) of
  (PrimitiveType mut I32, IntLiteral _) -> PrimitiveType mut I32
  (PrimitiveType mut I64, IntLiteral _) -> PrimitiveType mut I64
  (PrimitiveType mut U32, IntLiteral _) -> PrimitiveType mut U32
  (PrimitiveType mut U64, IntLiteral _) -> PrimitiveType mut U64
  (PrimitiveType mut F32, IntLiteral _) -> PrimitiveType mut F32
  (PrimitiveType mut F64, IntLiteral _) -> PrimitiveType mut F64
  (PrimitiveType mut F32, FloatLiteral _) -> PrimitiveType mut F32
  (PrimitiveType mut F64, FloatLiteral _) -> PrimitiveType mut F64
  (_, IntLiteral _) -> PrimitiveType Immutable I32
  (_, FloatLiteral _) -> PrimitiveType Immutable F32

isNumericType :: Type -> Bool
isNumericType (PrimitiveType _ t) = case t of
  I8  -> True
  I16 -> True
  I32 -> True
  I64 -> True
  U8  -> True
  U16 -> True
  U32 -> True
  U64 -> True
  F32 -> True
  F64 -> True
isNumericType _ = False

isIntegerType :: Type -> Bool
isIntegerType (PrimitiveType _ t) = case t of
  I8  -> True
  I16 -> True
  I32 -> True
  I64 -> True
  U8  -> True
  U16 -> True
  U32 -> True
  U64 -> True
  _ -> False
isIntegerType _ = False

isLogicalType :: Type -> Bool
isLogicalType t = isIntegerType t

analyzeTypeDecl :: AnalysisContext -> String -> Type -> Either [AnalysisError] (AnalysisContext, [AnalysisError])
analyzeTypeDecl ctx name typ = do
  when (Map.member name (types ctx)) $
    Left [DuplicateDefinition $ "Type " ++ name ++ " already defined"]
    
  validatedType <- validateType ctx typ
  let newTypes = Map.insert name validatedType (types ctx)
  Right (ctx { types = newTypes }, [])
