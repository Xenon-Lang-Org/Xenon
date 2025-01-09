{-# LANGUAGE LambdaCase #-}
module FillModuleData (fillWASMModuleFromAST) where

import Parser.Data.Ast
import ModuleData
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
    , let -- gather function parameters + local variable declarations
          localDecls  = collectLocalVars body
          allLocals   = params ++ localDecls
          localVarMap = buildLocalVariableMap allLocals
          bodyInstrs  = concatMap (toInstruction localVarMap globalMap functionMap) body
    ]
  where
    funDecls    = [fd | fd@(FunctionDeclaration _ _ _ _) <- stmts]
    functionMap = buildFunctionMap stmts

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

initializeGlobal :: ValueType -> Maybe Expression -> [Instruction]
initializeGlobal vt = \case
  Just (ELiteral (IntLiteral val)) ->
    [mkConst vt (fromIntegral val)]

  Just (ELiteral (FloatLiteral fval)) ->
    [mkConst vt (realToFrac fval)]

  _ -> [mkConst vt 0]

collectGlobals :: [Statement] -> [(ValueType, Mutability, [Instruction])]
collectGlobals stmts =
    [ (vt, Mutable, initializeGlobal vt maybeExpr)
    | VariableDeclaration _ typ maybeExpr <- stmts
    , let vt = toValueType typ
    ]

collectLocalVars :: [Statement] -> [(String, Type)]
collectLocalVars stmts = concatMap (\stmt -> case stmt of
    VariableDeclaration name t _ -> [(name, t)]
    _                            -> []) stmts

mkConst :: ValueType -> Double -> Instruction
mkConst vt x = case vt of
  ModuleI32 -> ModuleConstI32 (floor x)
  ModuleI64 -> ModuleConstI64 (floor x)
  ModuleF32 -> ModuleConstF32 (realToFrac x)
  ModuleF64 -> ModuleConstF64 x

buildGlobalMap :: [Statement] -> [(String, ValueType, Int)]
buildGlobalMap stmts =
    zipWith (\(n, t) i -> (n, t, i)) varDecls [0..]
  where
    varDecls =
      [ (name, toValueType typ)
      | VariableDeclaration name typ _ <- stmts
      ]

buildLocalVariableMap :: [(String, Type)] -> [(String, Int)]
buildLocalVariableMap params =
    zip (map fst params) [0..]

buildFunctionMap :: [Statement] -> [(String, Int)]
buildFunctionMap stmts =
    zip functionNames [0..]
  where
    functionNames =
      [ name
      | FunctionDeclaration name _ _ _ <- stmts
      ]

toValueType :: Type -> ValueType
toValueType (PrimitiveType I32) = ModuleI32
toValueType (PrimitiveType I64) = error "I64 WIP"
toValueType (PrimitiveType F32) = error "F32 WIP"
toValueType (PrimitiveType F64) = error "F64 WIP"
toValueType _ = error "Unsupported type"

toInstruction
    :: [(String, Int)]             -- localVarMap
    -> [(String, ValueType, Int)]  -- globalMap
    -> [(String, Int)]             -- functionMap
    -> Statement
    -> [Instruction]

-- ExpressionStatement
toInstruction locMap gMap funcMap (ExpressionStatement expr) =
    toInstructionFromExpr locMap gMap funcMap expr

-- ReturnStatement
toInstruction locMap gMap funcMap (ReturnStatement expr) =
    toInstructionFromExpr locMap gMap funcMap expr
    ++ [ModuleReturn]

-- If statement  WIP
toInstruction locMap gMap funcMap (Conditional cond thenStmts maybeElse) =
    let condInstrs = toInstructionFromExpr locMap gMap funcMap cond
        thenInstrs = concatMap (toInstruction locMap gMap funcMap) thenStmts
        elseInstrs = case maybeElse of
          Just (elseExpr, elseStmts, _) ->
             toInstructionFromExpr locMap gMap funcMap elseExpr
            ++ concatMap (toInstruction locMap gMap funcMap) elseStmts
          Nothing -> []
    in condInstrs ++ [ModuleIf Nothing thenInstrs elseInstrs]

-- WhileLoop
toInstruction locMap gMap funcMap (WhileLoop cond bodyStmts) =
  let condInstrs = toInstructionFromExpr locMap gMap funcMap cond
      bodyInstrs = concatMap (toInstruction locMap gMap funcMap) bodyStmts
  in
  [ ModuleBlock Nothing  -- Outer block for breaking
      [ ModuleLoop Nothing  -- Loop block
          ( condInstrs                   -- (x < a)? => 1 or 0 on stack
         ++ [ ModuleEqz ModuleI32        -- invert: 1 -> 0 (false), 0 -> 1 (true)
            , ModuleBrIf 1               -- if top==1 => break
            ]
         ++ bodyInstrs                   -- loop body
         ++ [ ModuleBr 0 ]               -- jump back to start of loop
          )
      ]
  ]

toInstruction _ _ _ _ = []

toInstructionFromExpr
    :: [(String, Int)]
    -> [(String, ValueType, Int)]
    -> [(String, Int)]
    -> Expression
    -> [Instruction]
toInstructionFromExpr locMap gMap funcMap = \case

  Variable varName ->
    case lookupVar locMap varName of
      Just localIdx  -> [ModuleLocalGet localIdx]
      Nothing        -> case lookupGlobal gMap varName of
          Just (globalIdx, _) -> [ModuleGlobalGet globalIdx]
          Nothing        -> error $ "Unknown variable: " ++ varName

  ELiteral (IntLiteral i)   -> [ModuleConstI32 i]
  ELiteral (FloatLiteral f) -> [ModuleConstF32 f]

  UnaryOp Negate expr ->
    let innerInstrs = toInstructionFromExpr locMap gMap funcMap expr
        vt = detectValueType expr locMap gMap
    in innerInstrs ++ case vt of
      ModuleI32 -> [ModuleEqz ModuleI32]
      ModuleI64 -> [ModuleEqz ModuleI64]
      ModuleF32 -> [ModuleEqz ModuleF32]
      ModuleF64 -> [ModuleEqz ModuleF64]

  BinaryOp op left right -> 
    case op of
      Assign -> compileAssignment locMap gMap funcMap left right
      _ ->
        let leftInstrs  = toInstructionFromExpr locMap gMap funcMap left
            rightInstrs = toInstructionFromExpr locMap gMap funcMap right
            vt          = detectValueType left locMap gMap
            binInstr    = mkBinOpInstruction op vt
        in leftInstrs ++ rightInstrs ++ [binInstr]


  FunctionCall funcName args ->
    concatMap (toInstructionFromExpr locMap gMap funcMap) args
      ++ [ModuleCall (lookupFunc funcMap funcName)]

  _ -> []

compileAssignment :: [(String, Int)]  -- localVarMap
    -> [(String, ValueType, Int)]  -- globalMap
    -> [(String, Int)]             -- functionMap (unused here)
    -> Expression                  -- Left-hand side
    -> Expression                  -- Right-hand side
    -> [Instruction]
compileAssignment locMap gMap _ lhs rhs =
  let rhsInstrs = toInstructionFromExpr locMap gMap [] rhs
  in case lhs of
      Variable name ->
        case lookupVar locMap name of
          Just localIdx -> rhsInstrs ++ [ModuleLocalSet localIdx]
          Nothing ->
            case lookupGlobal gMap name of
              Just (globalIdx, _) -> rhsInstrs ++ [ModuleGlobalSet globalIdx]
              Nothing -> error $ "Unknown variable for assignment: " ++ name
      _ -> error "Left-hand side of assignment must be a variable"


detectValueType :: Expression -> [(String, Int)] -> [(String, ValueType, Int)] -> ValueType
detectValueType expr locMap gMap = case expr of
  Variable varName ->
    case lookupVar locMap varName of
      Just _     -> ModuleI32  -- Assuming locals are i32 by default
      Nothing    -> case lookupGlobal gMap varName of
        Just (_, vt) -> vt
        Nothing      -> error $ "Unknown variable: " ++ varName
  ELiteral (IntLiteral _)   -> ModuleI32
  ELiteral (FloatLiteral _) -> ModuleF32
  _ -> error $ "Unable to detect ValueType for: " ++ show expr

mkBinOpInstruction :: BinOp -> ValueType -> Instruction
mkBinOpInstruction op vt = case op of
  Add -> ModuleAdd vt
  Sub -> ModuleSub vt
  Mul -> ModuleMul vt
  Div -> ModuleDiv vt
  Gt  -> ModuleGt  vt
  Lt  -> ModuleLt  vt
  Eq  -> ModuleEq  vt
  Neq -> ModuleNeq vt
  Le  -> ModuleLe  vt
  Ge  -> ModuleGe  vt
  _   -> error $ "Unsupported combination: " ++ show op

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