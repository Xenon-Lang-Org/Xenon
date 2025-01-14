{-# LANGUAGE LambdaCase #-}

module Compiler.System.FillModuleData (fillWASMModuleFromAST) where

import Parser.Data.Ast
import Compiler.Data.ModuleData
import Data.Maybe (fromMaybe)

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
    | (i, FunctionDeclaration _ params _ body) <- zip [0..] funDecls
    , let localDecls  = collectLocalVars body
          allLocals   = params ++ localDecls
          localVarMap = buildLocalVariableMap allLocals
          bodyInstrs  = concatMap (toInstruction localVarMap globalMap functionMap) body
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
    | FunctionDeclaration _ params _ body <- funDecls
    , let localDecls  = collectLocalVars body
          allLocals   = params ++ localDecls
          localVarMap = buildLocalVariableMap allLocals
          bodyInstrs  = concatMap (toInstruction localVarMap globalMap functionMap) body
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
    :: [(String, (ValueType, Int))]  -- localVarMap
    -> [(String, ValueType, Int)]    -- globalMap
    -> [(String, Int)]               -- functionMap
    -> Statement
    -> [Instruction]

-- ReturnStatement
toInstruction locMap gMap funcMap (ReturnStatement expr) =
    toInstructionFromExpr locMap gMap funcMap expr
    ++ [ModuleReturn]

-- WhileLoop
toInstruction locMap gMap funcMap (WhileLoop cond bodyStmts) =
  let condInstrs = toInstructionFromExpr locMap gMap funcMap cond
      bodyInstrs = concatMap (toInstruction locMap gMap funcMap) bodyStmts
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
toInstruction locMap gMap funcMap (If cond thenBody maybeElseBody) =
    let condInstrs = toInstructionFromExpr locMap gMap funcMap cond
        thenInstrs = concatMap (toInstruction locMap gMap funcMap) thenBody
        elseInstrs = maybe [] (concatMap (toInstruction locMap gMap funcMap)) maybeElseBody
    in condInstrs ++ [ModuleIf Nothing thenInstrs elseInstrs]

-- TypeDeclaration
toInstruction _ _ _ (TypeDeclaration _ _) =
  []

-- StandaloneFunctionCall
toInstruction locMap gMap funcMap (StandaloneFunctionCall funcName args) =
    concatMap (toInstructionFromExpr locMap gMap funcMap) args
    ++ [ModuleCall (lookupFunc funcMap funcName)]

-- VariableReAssignment
toInstruction locMap gMap _ (VariableReAssignment name expr) =
  let rhsInstrs = toInstructionFromExpr locMap gMap [] expr
  in case lookupVar locMap name of
       Just (_, localIdx) -> rhsInstrs ++ [ModuleLocalSet localIdx]
       Nothing ->
         case lookupGlobal gMap name of
           Just (globalIdx, _) -> rhsInstrs ++ [ModuleGlobalSet globalIdx]
           Nothing -> error $ "Unknown variable for assignment: " ++ name

-- VariableDeclaration in top-level statements
toInstruction locMap gMap funcMap (VariableDeclaration name _ maybeExpr) =
  case lookupVar locMap name of
    Just (_, localIdx) ->
      let initInstrs = maybe [] (toInstructionFromExpr locMap gMap funcMap) maybeExpr
      in initInstrs ++ [ModuleLocalSet localIdx]
    Nothing ->
      case lookupGlobal gMap name of
        Just (globalIdx, _) ->
          let initInstrs = maybe [] (toInstructionFromExpr locMap gMap funcMap) maybeExpr
          in initInstrs ++ [ModuleGlobalSet globalIdx]
        Nothing -> error $ "Unknown variable for declaration: " ++ name

-- FunctionDeclaration
toInstruction _ _ _ (FunctionDeclaration {}) = []

toInstructionFromExpr
    :: [(String, (ValueType, Int))]
    -> [(String, ValueType, Int)]
    -> [(String, Int)]
    -> Expression
    -> [Instruction]
toInstructionFromExpr locMap gMap funcMap = \case

  -- Variable
  Variable varName ->
    case lookupVar locMap varName of
      Just (_, localIdx)  -> [ModuleLocalGet localIdx]
      Nothing        -> case lookupGlobal gMap varName of
          Just (globalIdx, _) -> [ModuleGlobalGet globalIdx]
          Nothing             -> error $ "Unknown variable: " ++ varName

  -- Literals
  ELiteral (IntLiteral i)   -> [ModuleConstI32 (fromInteger i)]
  ELiteral (FloatLiteral f) -> [ModuleConstF32 (realToFrac f)]

  -- UnaryOp
  UnaryOp op expr ->
    let exprInstrs = toInstructionFromExpr locMap gMap funcMap expr
        vt         = detectValueType expr locMap gMap
        unaryInstr = mkUnaryOpInstruction op vt
    in exprInstrs ++ [unaryInstr]

  -- BinaryOp
  BinaryOp op left right ->
    let leftInstrs  = toInstructionFromExpr locMap gMap funcMap left
        rightInstrs = toInstructionFromExpr locMap gMap funcMap right
        vt          = detectValueType left locMap gMap
        binInstr    = mkBinOpInstruction op vt
    in leftInstrs ++ rightInstrs ++ [binInstr]

  -- FunctionCall
  FunctionCall funcName args ->
    concatMap (toInstructionFromExpr locMap gMap funcMap) args
      ++ [ModuleCall (lookupFunc funcMap funcName)]

  -- Parenthesis
  Parenthesis expr ->
    toInstructionFromExpr locMap gMap funcMap expr

detectValueType
    :: Expression
    -> [(String, (ValueType, Int))]
    -> [(String, ValueType, Int)]
    -> ValueType
detectValueType expr locMap globalMap =
  case expr of
    Variable varName ->
      case lookupVar locMap varName of
        Just (vt, _) -> vt
        Nothing -> case lookupGlobal globalMap varName of
          Just (_, vt) -> vt
          Nothing      -> error $ "Unknown variable: " ++ varName

    ELiteral (IntLiteral _)   -> ModuleI32

    ELiteral (FloatLiteral _) -> ModuleF32

    BinaryOp _ left _ -> detectValueType left locMap globalMap

    _ -> error "Unsupported expression"

mkBinOpInstruction :: BinOp -> ValueType -> Instruction
mkBinOpInstruction op vt = case op of
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

mkUnaryOpInstruction :: UnaryOp -> ValueType -> Instruction
mkUnaryOpInstruction op vt = case op of
  Negate -> ModuleEqz vt
  BitNot -> ModuleBitNot vt
  _ -> error $ "Unsupported unary op: " ++ show op

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
