module Analyzer.IR
  ( -- * Types
    IR(..)
  , IRFunction(..)
  , IRInst(..)
  , IRType(..)
  , IRValue(..)
    -- * Core Functions
  , astToIR
  , optimizeIR
    -- * Optimization Functions
  , constantFolding
  , deadCodeElimination
  ) where

import qualified Data.Map()
import Parser.Data.Ast
import Data.List (nub)

-------------------------------------------------------------------------------
-- | Complete IR program representation
-------------------------------------------------------------------------------
data IR = IR 
  { irGlobals :: [(String, IRType)]           -- Global variables
  , irFunctions :: [(String, IRFunction)]      -- Function definitions
  , irTypes :: [(String, IRType)]             -- Custom type definitions
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | IR Function representation
-------------------------------------------------------------------------------
data IRFunction = IRFunction 
  { irParams :: [(String, IRType)]            -- Parameter names and types
  , irLocals :: [(String, IRType)]            -- Local variable declarations
  , irBody :: [IRInst]                        -- Instruction sequence
  , irReturnType :: IRType                    -- Return type
  } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | IR Instructions
-------------------------------------------------------------------------------
data IRInst
  = IRMove String IRValue                     -- Assign value to variable
  | IRBinOp String IRValue BinOp IRValue      -- Binary operation
  | IRUnOp String UnaryOp IRValue             -- Unary operation
  | IRCall String String [IRValue]            -- Function call
  | IRReturn (Maybe IRValue)                  -- Return statement
  | IRJump String                             -- Unconditional jump
  | IRLabel String                            -- Label definition
  | IRCond IRValue String String              -- Conditional branch
  | IRPhi String [(String, IRValue)]          -- PHI node for SSA
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | IR Types
-------------------------------------------------------------------------------
data IRType
  = IRInt Int                                 -- Integer types
  | IRFloat Int                               -- Float types
  | IRPtr IRType                              -- Pointer types
  | IRStruct [(String, IRType)]               -- Struct types
  | IRArray Int IRType                        -- Fixed-size arrays
  | IRVoid                                    -- Void type
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | IR Values
-------------------------------------------------------------------------------
data IRValue
  = IRVar String                              -- Variable reference
  | IRIntLit Integer                          -- Integer literal
  | IRFloatLit Double                         -- Float literal
  | IRTemp String                             -- Temporary variable
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- | Convert AST to IR
-------------------------------------------------------------------------------
astToIR :: Program -> IR
astToIR (Program stmts) = IR 
  { irGlobals = collectGlobals stmts
  , irFunctions = 
      case stmts of
        [stmt] -> [("main", generateMainFunction stmt)]  -- Convert single statement to main function
        _ -> collectFunctions stmts
  , irTypes = collectTypes stmts
  }

generateMainFunction :: Statement -> IRFunction
generateMainFunction stmt = IRFunction
  { irParams = []
  , irLocals = []
  , irBody = generateInstructions [stmt]
  , irReturnType = IRVoid
  }

-------------------------------------------------------------------------------
-- | Convert AST type to IR type
-------------------------------------------------------------------------------
astTypeToIRType :: Type -> IRType
astTypeToIRType (PrimitiveType _ I8) = IRInt 8
astTypeToIRType (PrimitiveType _ I16) = IRInt 16
astTypeToIRType (PrimitiveType _ I32) = IRInt 32
astTypeToIRType (PrimitiveType _ I64) = IRInt 64
astTypeToIRType (PrimitiveType _ U8) = IRInt 8
astTypeToIRType (PrimitiveType _ U16) = IRInt 16
astTypeToIRType (PrimitiveType _ U32) = IRInt 32
astTypeToIRType (PrimitiveType _ U64) = IRInt 64
astTypeToIRType (PrimitiveType _ F32) = IRFloat 32
astTypeToIRType (PrimitiveType _ F64) = IRFloat 64
astTypeToIRType (PointerType _ innerType) = IRPtr (astTypeToIRType innerType)
astTypeToIRType (StructType _ (Struct fields)) = 
    IRStruct [(name, astTypeToIRType typ) | (name, typ) <- fields]
astTypeToIRType (ArrayType _ (Array size typ)) = 
    IRArray size (astTypeToIRType typ)
astTypeToIRType _ = IRVoid

-------------------------------------------------------------------------------
-- | Collect global variables
-------------------------------------------------------------------------------
collectGlobals :: [Statement] -> [(String, IRType)]
collectGlobals stmts = 
  [(name, astTypeToIRType typ) | 
   VariableDeclaration name typ _ <- stmts,
   isGlobalScope stmts name]
  where
    isGlobalScope stmtes name = 
      not $ any isFunctionLocal stmtes
      where
        isFunctionLocal (FunctionDeclaration _ params _ _) = 
          name `elem` map fst params
        isFunctionLocal _ = False
-------------------------------------------------------------------------------
-- | Collect functions
-------------------------------------------------------------------------------
collectFunctions :: [Statement] -> [(String, IRFunction)]
collectFunctions stmts = 
  [(name, convertFunction params retType body) | 
   FunctionDeclaration name params retType body <- stmts]
  where
    convertFunction :: [Field] -> Type -> [Statement] -> IRFunction
    convertFunction params retType body = IRFunction
      { irParams = [(name, astTypeToIRType typ) | (name, typ) <- params]
      , irLocals = collectLocals body
      , irBody = generateInstructions body
      , irReturnType = astTypeToIRType retType
      }

    collectLocals :: [Statement] -> [(String, IRType)]
    collectLocals stmtes = 
      [(name, astTypeToIRType typ) | 
       VariableDeclaration name typ _ <- stmtes]

-------------------------------------------------------------------------------
-- | Generate IR instructions from statements
-------------------------------------------------------------------------------
generateInstructions :: [Statement] -> [IRInst]
generateInstructions = concatMap convertStatement
  where
    convertStatement :: Statement -> [IRInst]
    convertStatement (WhileLoop cond body) = 
      let startLabel = "while_start_" ++ show (hash cond)
          bodyLabel = "while_body_" ++ show (hash cond)
          endLabel = "while_end_" ++ show (hash cond)
          condInsts = generateExpr cond
      in [ IRLabel startLabel ]
         ++ condInsts
         ++ [ IRCond (IRTemp (lastTemp condInsts)) bodyLabel endLabel
            , IRLabel bodyLabel ]
         ++ concatMap convertStatement body
         ++ [ IRJump startLabel
            , IRLabel endLabel ]
    
    convertStatement (If cond thenBody mElseBody) =
      let thenLabel = "if_then_" ++ show (hash cond)
          elseLabel = "if_else_" ++ show (hash cond)
          endLabel = "if_end_" ++ show (hash cond)
          condInsts = generateExpr cond
      in condInsts
         ++ [ IRCond (IRTemp (lastTemp condInsts)) thenLabel elseLabel
            , IRLabel thenLabel ]
         ++ concatMap convertStatement thenBody
         ++ [ IRJump endLabel
            , IRLabel elseLabel ]
         ++ maybe [] (concatMap convertStatement) mElseBody
         ++ [ IRLabel endLabel ]

    convertStatement (VariableDeclaration name _ (Just expr)) =
      let exprInsts = generateExpr expr
      in exprInsts ++ [IRMove name (IRTemp (lastTemp exprInsts))]

    convertStatement (ReturnStatement expr) =
      let exprInsts = generateExpr expr
      in exprInsts ++ [IRReturn (Just (IRTemp (lastTemp exprInsts)))]
    
    convertStatement _ = []

-------------------------------------------------------------------------------
-- | Generate IR instructions from expressions
-------------------------------------------------------------------------------
generateExpr :: Expression -> [IRInst]
generateExpr expr = case expr of
  BinaryOp op e1 e2 ->
    let e1Insts = generateExpr e1
        e2Insts = generateExpr e2
        temp = freshTemp expr
    in e1Insts ++ e2Insts ++ 
       [IRBinOp temp (IRTemp (lastTemp e1Insts)) op (IRTemp (lastTemp e2Insts))]
  
  UnaryOp op e ->
    let eInsts = generateExpr e
        temp = freshTemp expr
    in eInsts ++ [IRUnOp temp op (IRTemp (lastTemp eInsts))]
  
  Variable name ->
    let temp = freshTemp expr
    in [IRMove temp (IRVar name)]
  
  ELiteral (IntLiteral n) ->
    let temp = freshTemp expr
    in [IRMove temp (IRIntLit n)]
  
  ELiteral (FloatLiteral f) ->
    let temp = freshTemp expr
    in [IRMove temp (IRFloatLit f)]
  
  _ -> []

-------------------------------------------------------------------------------
-- | Collect custom type definitions
-------------------------------------------------------------------------------
collectTypes :: [Statement] -> [(String, IRType)]
collectTypes stmts =
  [(name, astTypeToIRType typ) | TypeDeclaration name typ <- stmts]

-------------------------------------------------------------------------------
-- | Optimize IR code
-------------------------------------------------------------------------------
optimizeIR :: IR -> IR
optimizeIR = deadCodeElimination . constantFolding

-------------------------------------------------------------------------------
-- | Constant folding optimization
-------------------------------------------------------------------------------
constantFolding :: IR -> IR
constantFolding ir = ir { irFunctions = map foldFunction (irFunctions ir) }
  where
    foldFunction :: (String, IRFunction) -> (String, IRFunction)
    foldFunction (name, func) = (name, func { irBody = foldInstructions (irBody func) })

    foldInstructions :: [IRInst] -> [IRInst]
    foldInstructions insts = map foldInstruction insts

    foldInstruction :: IRInst -> IRInst
    foldInstruction (IRBinOp dest (IRIntLit v1) op (IRIntLit v2)) =
      IRMove dest (IRIntLit $ evalBinOp op v1 v2)
    foldInstruction inst = inst

    evalBinOp :: BinOp -> Integer -> Integer -> Integer
    evalBinOp Add v1 v2 = v1 + v2
    evalBinOp Sub v1 v2 = v1 - v2
    evalBinOp Mul v1 v2 = v1 * v2
    evalBinOp Div v1 v2 = v1 `div` v2
    evalBinOp _ v1 _ = v1

-------------------------------------------------------------------------------
-- | Dead code elimination
-------------------------------------------------------------------------------
deadCodeElimination :: IR -> IR
deadCodeElimination ir = ir { irFunctions = map eliminateDeadCode (irFunctions ir) }
  where
    eliminateDeadCode :: (String, IRFunction) -> (String, IRFunction)
    eliminateDeadCode (name, func) = 
      (name, func { irBody = filterLiveInsts (irBody func) })

    filterLiveInsts :: [IRInst] -> [IRInst]
    filterLiveInsts insts = 
      let used = gatherUsedVars insts
      in filter (isLiveInst used) insts

    gatherUsedVars :: [IRInst] -> [String]
    gatherUsedVars = nub . concatMap getUsedVars
      where
        getUsedVars (IRReturn (Just (IRVar v))) = [v]
        getUsedVars (IRBinOp _ (IRVar v1) _ (IRVar v2)) = [v1, v2]
        getUsedVars (IRBinOp _ _ _ (IRVar v)) = [v]
        getUsedVars (IRBinOp _ (IRVar v) _ _) = [v]
        getUsedVars (IRMove _ (IRVar v)) = [v]
        getUsedVars (IRCond (IRVar v) _ _) = [v]
        getUsedVars _ = []

    isLiveInst :: [String] -> IRInst -> Bool
    isLiveInst _ (IRLabel _) = True
    isLiveInst _ (IRJump _) = True
    isLiveInst _ (IRReturn _) = True
    isLiveInst _ (IRCond _ _ _) = True
    isLiveInst used (IRMove dest _) = dest `elem` used
    isLiveInst used (IRBinOp dest _ _ _) = dest `elem` used
    isLiveInst _ _ = False

-- -------------------------------------------------------------------------------
-- -- | Common subexpression elimination
-- -------------------------------------------------------------------------------
-- commonSubexpressionElimination :: IR -> IR
-- commonSubexpressionElimination ir = 
--   ir { irFunctions = map eliminateCSE (irFunctions ir) }
--   where
--     eliminateCSE (name, func) = 
--       (name, func { irBody = eliminateCommonSubexpr (irBody func) })

--     eliminateCommonSubexpr :: [IRInst] -> [IRInst]
--     eliminateCommonSubexpr insts =
--       let (exprs, optimized) = foldl findAndReplace (Map.empty, []) insts
--       in reverse optimized

--     findAndReplace :: (Map.Map String String, [IRInst]) -> IRInst -> (Map.Map String String, [IRInst])
--     findAndReplace (exprs, acc) inst@(IRBinOp dest v1 op v2) =
--       let key = show v1 ++ show op ++ show v2
--       in case Map.lookup key exprs of
--            Just existing -> (exprs, IRMove dest (IRVar existing) : acc)
--            Nothing -> (Map.insert key dest exprs, inst : acc)
--     findAndReplace (exprs, acc) inst = (exprs, inst : acc)

-------------------------------------------------------------------------------
-- | Helper functions for generating unique names
-------------------------------------------------------------------------------
-- freshLabel :: String -> String
-- freshLabel base = base ++ "_" ++ show (length base)

hash :: Show a => a -> Int
hash = length . show

freshTemp :: Show a => a -> String
freshTemp x = "_t" ++ show (hash x)

lastTemp :: [IRInst] -> String
lastTemp insts = case last insts of
  IRMove dest _ -> dest
  IRBinOp dest _ _ _ -> dest
  IRUnOp dest _ _ -> dest
  _ -> error "No temporary variable found"

-- getTempName :: Expression -> String
-- getTempName expr = "_t" ++ show (length (show expr))
