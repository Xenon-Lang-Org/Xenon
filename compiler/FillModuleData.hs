{-# LANGUAGE LambdaCase #-}

module FillModuleData (fillWASMModuleFromAST) where

import Parser.Data.Ast
import ModuleData
import Data.Maybe (fromMaybe)

-- Collect type definitions from function declarations
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

-- Build function map: (functionName -> functionIndex)
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
  Just (ELiteral (FloatLiteral fval)) -> [mkConst vt (realToFrac fval)]
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

-- Build local variable map: (varName -> localIndex)
buildLocalVariableMap :: [(String, Type)] -> [(String, Int)]
buildLocalVariableMap params = zip (map fst params) [0..]

toValueType :: Type -> ValueType
toValueType (PrimitiveType _ I32) = ModuleI32
toValueType (PrimitiveType _ I64) = error "I64 WIP"
toValueType (PrimitiveType _ F32) = error "F32 WIP"
toValueType (PrimitiveType _ F64) = error "F64 WIP"
toValueType _ = error "Unsupported type"

toInstruction
    :: [(String, Int)]             -- localVarMap
    -> [(String, ValueType, Int)]  -- globalMap
    -> [(String, Int)]             -- functionMap
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
  []  -- Type declarations generate no runtime instructions

-- StandaloneFunctionCall
toInstruction locMap gMap funcMap (StandaloneFunctionCall funcName args) =
    -- Evaluate each argument, then call the function
    concatMap (toInstructionFromExpr locMap gMap funcMap) args
    ++ [ModuleCall (lookupFunc funcMap funcName)]

-- VariableReAssignment
toInstruction locMap gMap _ (VariableReAssignment name expr) =
  let rhsInstrs = toInstructionFromExpr locMap gMap [] expr
  in case lookupVar locMap name of
       Just localIdx -> rhsInstrs ++ [ModuleLocalSet localIdx]
       Nothing ->
         case lookupGlobal gMap name of
           Just (globalIdx, _) -> rhsInstrs ++ [ModuleGlobalSet globalIdx]
           Nothing -> error $ "Unknown variable for assignment: " ++ name

-- VariableDeclaration in top-level statements
toInstruction _ _ _ (VariableDeclaration _ _ _) = []

-- FunctionDeclaration
toInstruction _ _ _ (FunctionDeclaration _ _ _ _) = []

toInstruction _ _ _ stmt =
  error $ "Unhandled statement: " ++ show stmt

toInstructionFromExpr
    :: [(String, Int)]
    -> [(String, ValueType, Int)]
    -> [(String, Int)]
    -> Expression
    -> [Instruction]
toInstructionFromExpr locMap gMap funcMap = \case

  -- Variable
  Variable varName ->
    case lookupVar locMap varName of
      Just localIdx  -> [ModuleLocalGet localIdx]
      Nothing        -> case lookupGlobal gMap varName of
          Just (globalIdx, _) -> [ModuleGlobalGet globalIdx]
          Nothing             -> error $ "Unknown variable: " ++ varName

  -- Literals
  ELiteral (IntLiteral i)   -> [ModuleConstI32 (fromInteger i)]
  ELiteral (FloatLiteral f) -> [ModuleConstF32 (realToFrac f)]

  -- UnaryOp
  UnaryOp Negate innerExpr ->
    let innerInstrs = toInstructionFromExpr locMap gMap funcMap innerExpr
        vt          = detectValueType innerExpr locMap gMap
    in innerInstrs ++ [ModuleEqz vt]

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

detectValueType :: Expression -> [(String, Int)] -> [(String, ValueType, Int)] -> ValueType
detectValueType expr locMap gMap = case expr of
  Variable varName ->
    case lookupVar locMap varName of
      Just _     -> ModuleI32  -- assume locals are i32
      Nothing    -> case lookupGlobal gMap varName of
        Just (_, vt) -> vt
        Nothing      -> error $ "Unknown variable: " ++ varName
  ELiteral (IntLiteral _)   -> ModuleI32
  ELiteral (FloatLiteral _) -> ModuleF32
  _ -> ModuleI32

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
  _ -> error $ "Unsupported binop for now: " ++ show op

lookupVar :: [(String, Int)] -> String -> Maybe Int
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

