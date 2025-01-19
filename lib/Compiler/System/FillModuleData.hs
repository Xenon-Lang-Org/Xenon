{-# LANGUAGE LambdaCase #-}

module Compiler.System.FillModuleData (fillWASMModuleFromAST) where

import Parser.Data.Ast
import Compiler.Data.ModuleData
import Data.Maybe (fromMaybe)
import VM.Data.AST (ValType)

import Control.Applicative ((<|>))

collectTypes :: [Statement] -> [TypeSection]
collectTypes stmts =
    [ TypeSection (FunctionType (map (toValueType . snd) params) [toValueType ret])
    | FunctionDeclaration _ params ret _ <- funDecls
    ]
  where
    funDecls = [fd | fd@(FunctionDeclaration _ _ _ _) <- stmts]

collectFunctions
    :: [Statement]
    -> [(String, ValueType, Int)]  -- globalMap
    -> [Function]
collectFunctions stmts globalMap =
    [ Function i (CodeSection (map (toValueType . snd) allLocals) bodyInstrs)
    | (i, FunctionDeclaration _ params rT body) <- zip [0..] funDecls
    , let localDecls  = collectLocalVars body
          allLocals   = params ++ localDecls
          localVarMap = buildLocalVariableMap allLocals
          bodyInstrs  = concatMap (toInstruction (Just (typeToValueType rT)) localVarMap globalMap functionMap) body
    ]
  where
    funDecls    = [fd | fd@(FunctionDeclaration _ _ _ _) <- stmts]
    functionMap = buildFunctionMap stmts

buildFunctionMap :: [Statement] -> [(String, Int)]
buildFunctionMap stmts =
    zip functionNames [0..]
  where
    functionNames =
      [ name
      | FunctionDeclaration name _ _ _ <- stmts
      ]

collectExports :: [Statement] -> [ExportSection]
collectExports stmts =
    [ ExportSection (Export name FunctionExport)
    | FunctionDeclaration name _ _ _ <- funDecls
    ]
  where
    funDecls = [fd | fd@(FunctionDeclaration _ _ _ _) <- stmts]

collectCodes
    :: [Statement]
    -> [(String, ValueType, Int)]  -- globalMap
    -> [CodeSection]
collectCodes stmts globalMap =
    [ CodeSection (map (toValueType . snd) allLocals) bodyInstrs
    | FunctionDeclaration _ params rT body <- funDecls
    , let localDecls  = collectLocalVars body
          allLocals   = params ++ localDecls
          localVarMap = buildLocalVariableMap allLocals
          bodyInstrs  = concatMap (toInstruction (Just (typeToValueType rT)) localVarMap globalMap functionMap) body
    ]
  where
    funDecls    = [fd | fd@(FunctionDeclaration _ _ _ _) <- stmts]
    functionMap = buildFunctionMap stmts

collectGlobals :: [Statement] -> [(ValueType, Mutability, [Instruction])]
collectGlobals stmts =
    [ (vt, ModuleMutable, initializeGlobal vt maybeExpr)
    | VariableDeclaration _ typ maybeExpr <- stmts
    , let vt = toValueType typ
    ]

initializeGlobal :: ValueType -> Maybe Expression -> [Instruction]
initializeGlobal vt = \case
  Just (ELiteral (IntLiteral val))   -> [mkConst vt (fromIntegral val)]
  Just (ELiteral (FloatLiteral fval)) -> [mkConst vt fval]
  _ -> [mkConst vt 0]

mkConst :: ValueType -> Double -> Instruction
mkConst vt x = case vt of
  ModuleI32 -> ModuleConstI32 (floor x)
  ModuleI64 -> ModuleConstI64 (floor x)
  ModuleF32 -> ModuleConstF32 (realToFrac x)
  ModuleF64 -> ModuleConstF64 x

collectLocalVars :: [Statement] -> [(String, Type)]
collectLocalVars stmts = concatMap (\stmt -> case stmt of
    VariableDeclaration name t _ -> [(name, t)]
    _ -> []
  ) stmts

buildLocalVariableMap :: [(String, Type)] -> [(String, (ValueType, Int))]
buildLocalVariableMap params =
    zipWith (\(name, ty) idx -> (name, (toValueType ty, idx))) params [0..]

toValueType :: Type -> ValueType
toValueType (PrimitiveType _ I32) = ModuleI32
toValueType (PrimitiveType _ I64) = ModuleI64
toValueType (PrimitiveType _ F32) = ModuleF32
toValueType (PrimitiveType _ F64) = ModuleF64
toValueType _ = error "Unsupported type"

toInstruction
    :: Maybe ValueType               -- return type
    -> [(String, (ValueType, Int))]  -- localVarMap
    -> [(String, ValueType, Int)]    -- globalMap
    -> [(String, Int)]               -- functionMap
    -> Statement
    -> [Instruction]

-- ReturnStatement
toInstruction mT locMap gMap funcMap (ReturnStatement expr) =
    toInstructionFromExpr (detectValueType mT expr locMap gMap) locMap gMap funcMap expr
    ++ [ModuleReturn]

-- WhileLoop
toInstruction mT locMap gMap funcMap (WhileLoop cond bodyStmts) =
  let condInstrs = toInstructionFromExpr (detectValueType Nothing cond locMap gMap) locMap gMap funcMap cond
      bodyInstrs = concatMap (toInstruction mT locMap gMap funcMap) bodyStmts
  in
  [ ModuleBlock Nothing  -- Outer block for breaking
      [ ModuleLoop Nothing  -- Loop block
          ( condInstrs                   -- (while condition)
         ++ [ ModuleEqz ModuleI32        -- eqz => 1 -> 0, 0 -> 1
            , ModuleBrIf 1               -- if top==1 => break
            ]
         ++ bodyInstrs                   -- loop body
         ++ [ ModuleBr 0 ]               -- jump back to loop start
          )
      ]
  ]

-- If statement
toInstruction mT locMap gMap funcMap (If cond thenBody maybeElseBody) =
    let condInstrs = toInstructionFromExpr (detectValueType Nothing cond locMap gMap) locMap gMap funcMap cond
        thenInstrs = concatMap (toInstruction mT locMap gMap funcMap) thenBody
        elseInstrs = maybe [] (concatMap (toInstruction mT locMap gMap funcMap)) maybeElseBody
    in condInstrs ++ [ModuleIf Nothing thenInstrs elseInstrs]

-- TypeDeclaration
toInstruction _ _ _ _ (TypeDeclaration _ _) =
  []

-- StandaloneFunctionCall
toInstruction mT locMap gMap funcMap (StandaloneFunctionCall funcName args) =
    concatMap (toInstructionFromExpr Nothing locMap gMap funcMap) args
    ++ [ModuleCall (lookupFunc funcMap funcName)]

-- VariableReAssignment
toInstruction mT locMap gMap funcMap (VariableReAssignment name expr) =
  case lookupVar locMap name of
    Just (vt, localIdx) -> toInstructionFromExpr (Just vt) locMap gMap funcMap expr ++ [ModuleLocalSet localIdx]
    Nothing ->
      case lookupGlobal gMap name of
        Just (globalIdx, vt) -> toInstructionFromExpr (Just vt) locMap gMap funcMap expr ++ [ModuleGlobalSet globalIdx]
        Nothing -> error $ "Unknown variable for assignment: " ++ name

-- VariableDeclaration in top-level statements
toInstruction mT locMap gMap funcMap (VariableDeclaration name _ maybeExpr) =
  case lookupVar locMap name of
    Just (vt, localIdx) ->
      let initInstrs = maybe [] (toInstructionFromExpr (Just vt) locMap gMap funcMap) maybeExpr
      in initInstrs ++ [ModuleLocalSet localIdx]
    Nothing ->
      case lookupGlobal gMap name of
        Just (globalIdx, vt) ->
          let initInstrs = maybe [] (toInstructionFromExpr (Just vt) locMap gMap funcMap) maybeExpr
          in initInstrs ++ [ModuleGlobalSet globalIdx]
        Nothing -> error $ "Unknown variable for declaration: " ++ name

-- FunctionDeclaration
toInstruction _ _ _ _ (FunctionDeclaration {}) = []

toInstructionFromExpr
    :: 
    Maybe ValueType
    -> [(String, (ValueType, Int))]
    -> [(String, ValueType, Int)]
    -> [(String, Int)]
    -> Expression
    -> [Instruction]
toInstructionFromExpr (Just ModuleI32) _ _ _ (ELiteral (IntLiteral i)) = [ModuleConstI32 (fromInteger i)]
toInstructionFromExpr (Just ModuleI64) _ _ _ (ELiteral (IntLiteral i)) = [ModuleConstI64 (fromInteger i)]
toInstructionFromExpr (Just ModuleF32) _ _ _ (ELiteral (FloatLiteral f)) = [ModuleConstF32 (realToFrac f)]
toInstructionFromExpr (Just ModuleF64) _ _ _ (ELiteral (FloatLiteral f)) = [ModuleConstF64 f]
toInstructionFromExpr _ locMap gMap _ (Variable varName) =
  case lookupVar locMap varName of
    Just (_, localIdx)  -> [ModuleLocalGet localIdx]
    Nothing        -> case lookupGlobal gMap varName of
        Just (globalIdx, _) -> [ModuleGlobalGet globalIdx]
        Nothing             -> error $ "Unknown variable: " ++ varName
toInstructionFromExpr mT locMap gMap funcMap (UnaryOp op expr) =
  let exprInstrs = toInstructionFromExpr mT locMap gMap funcMap expr
      vt         = detectValueType Nothing expr locMap gMap
      unaryInstr = mkUnaryOpInstruction op vt
  in exprInstrs ++ [unaryInstr]
toInstructionFromExpr mT locMap gMap funcMap (BinaryOp op left right) =
  let leftInstrs  = toInstructionFromExpr mT locMap gMap funcMap left
      rightInstrs = toInstructionFromExpr mT locMap gMap funcMap right
      vt          = detectValueType mT left locMap gMap <|> detectValueType mT right locMap gMap
      binInstr    = if exprIsBoolOp left || exprIsBoolOp right then mkBinOpInstruction op (Just ModuleI32) else mkBinOpInstruction op vt
  in leftInstrs ++ rightInstrs ++ [binInstr]
toInstructionFromExpr mT locMap gMap funcMap (FunctionCall funcName args) =
  concatMap (toInstructionFromExpr mT locMap gMap funcMap) args
    ++ [ModuleCall (lookupFunc funcMap funcName)]
toInstructionFromExpr mT locMap gMap funcMap (Parenthesis expr) =
  toInstructionFromExpr mT locMap gMap funcMap expr
toInstructionFromExpr _ _ _ _ (ELiteral _) = error "ValueType could not be detected in toInstructionFromExpr"

detectValueType
    :: 
    Maybe ValueType
    -> Expression
    -> [(String, (ValueType, Int))]
    -> [(String, ValueType, Int)]
    -> Maybe ValueType
detectValueType (Just vt) bop@(BinaryOp op _ _) a b = 
  if binOpIsBoolOp op then detectValueType Nothing bop a b else Just vt
detectValueType (Just vt) _ _ _ = Just vt
detectValueType _ expr locMap globalMap =
  case expr of
    Variable varName ->
      case lookupVar locMap varName of
        Just (vt, _) -> Just vt
        Nothing -> case lookupGlobal globalMap varName of
          Just (_, vt) -> Just vt
          Nothing      -> error $ "Unknown variable: " ++ varName

    ELiteral (IntLiteral _)   -> Nothing

    ELiteral (FloatLiteral _) -> Nothing

    BinaryOp _ left right -> detectValueType Nothing right locMap globalMap 
      <|> detectValueType Nothing left locMap globalMap

    _ -> error $ "Unsupported expression" ++ show expr

mkBinOpInstruction :: BinOp -> Maybe ValueType -> Instruction
mkBinOpInstruction _ Nothing = error "ValueType could not be detected in mkBinOpInstruction"
mkBinOpInstruction op (Just vt) = case op of
  Add    -> ModuleAdd vt
  Sub    -> ModuleSub vt
  Mul    -> ModuleMul vt
  Div    -> ModuleDiv vt
  Mod    -> ModuleMod vt
  Gt     -> ModuleGt  vt
  Lt     -> ModuleLt  vt
  Eq     -> ModuleEq  vt
  Neq    -> ModuleNeq vt
  Le     -> ModuleLe  vt
  Ge     -> ModuleGe  vt
  BitAnd -> ModuleBitAnd vt
  BitOr  -> ModuleBitOr  vt
  BitXor -> ModuleBitXor vt
  Shl    -> ModuleShl    vt
  Shr    -> ModuleShr    vt
  And    -> ModuleAnd    vt
  Or     -> ModuleOr     vt

binOpIsBoolOp :: BinOp -> Bool
binOpIsBoolOp op = case op of
  Gt  -> True
  Lt  -> True
  Eq  -> True
  Neq -> True
  Le  -> True
  Ge  -> True
  _   -> False

exprIsBoolOp :: Expression -> Bool
exprIsBoolOp (BinaryOp op _ _) = binOpIsBoolOp op
exprIsBoolOp _ = False

mkUnaryOpInstruction :: UnaryOp -> Maybe ValueType -> Instruction
mkUnaryOpInstruction op (Just vt) = case op of
  Negate -> ModuleEqz vt
  BitNot -> ModuleBitNot vt
  _ -> error $ "Unsupported unary op: " ++ show op
mkUnaryOpInstruction _ Nothing = error "ValueType could not be detected in mkUnaryOpInstruction"

lookupVar :: [(String, (ValueType, Int))] -> String -> Maybe (ValueType, Int)
lookupVar locMap name = lookup name locMap

lookupGlobal :: [(String, ValueType, Int)] -> String -> Maybe (Int, ValueType)
lookupGlobal gMap name =
    case filter (\(n, _, _) -> n == name) gMap of
      [(_, vt, idx)] -> Just (idx, vt)
      _              -> Nothing

lookupFunc :: [(String, Int)] -> String -> Int
lookupFunc funcMap name =
    fromMaybe (error $ "Unknown function: " ++ name) (lookup name funcMap)

fillWASMModuleFromAST :: Program -> WASMModule
fillWASMModuleFromAST (Program statements) =
    WASMModule
        { types     = collectTypes statements
        , imports   = []  -- WIP
        , functions = collectFunctions statements globalMap
        , exports   = collectExports statements
        , codes     = collectCodes statements globalMap
        , memories  = []  -- WIP
        , globals   = collectGlobals statements
        }
  where
    globalMap = buildGlobalMap statements

buildGlobalMap :: [Statement] -> [(String, ValueType, Int)]
buildGlobalMap stmts =
    zipWith (\(n, vt) i -> (n, vt, i)) varDecls [0..]
  where
    varDecls =
      [ (name, toValueType typ)
      | VariableDeclaration name typ _ <- stmts
      ]

typeToValueType :: Type -> ValueType
typeToValueType = \case
  PrimitiveType _ I32 -> ModuleI32
  PrimitiveType _ I64 -> ModuleI64
  PrimitiveType _ F32 -> ModuleF32
  PrimitiveType _ F64 -> ModuleF64
  _ -> error "Unsupported type"