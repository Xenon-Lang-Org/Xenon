module VM.Data.AST
  ( Value(..)
  , Instruction(..)
  , WasmFunction(..)
  , FuncType(..)
  , Export(..)
  , Module(..)
  , Global(..)
  , ValType(..)
  ) where

import Data.Int ( Int32, Int64 )

data Value
  = I32 Int32
  | I64 Int64
  | F32 Float
  | F64 Double
  deriving (Show, Eq)

data Instruction
  =
  -- CONST
    I32Const Int32
  | I64Const Int64
  | F32Const Float
  | F64Const Double

  -- MATH
  | I32Add
  | I64Add
  | F32Add
  | F64Add

  | I32Sub
  | I64Sub
  | F32Sub
  | F64Sub

  | I32Mul
  | I64Mul
  | F32Mul
  | F64Mul

  | I32DivS
  | I64DivS
  | F32Div
  | F64Div

  | I32RemS
  | I64RemS

  | I32And
  | I64And

  | I32Or
  | I64Or

  | I32Xor
  | I64Xor

  | I32Not
  | I64Not

  | I32Shl
  | I64Shl

  | I32Shr
  | I64Shr

  | I32GtS
  | I64GtS
  | F32Gt
  | F64Gt

  | I32LtS
  | I64LtS
  | F32Lt
  | F64Lt

  | I32Eq
  | I64Eq
  | F32Eq
  | F64Eq

  | I32Ne
  | I64Ne
  | F32Ne
  | F64Ne

  | I32GeS
  | I64GeS
  | F32Ge
  | F64Ge

  | I32LeS
  | I64LeS
  | F32Le
  | F64Le

  | I32Eqz
  | I64Eqz

  -- MEMORY
  | LocalGet Int
  | LocalSet Int
  | GlobalGet Int
  | GlobalSet Int

  -- CONTROL
  | Call Int

  | If
  | Else

  | Block

  | Loop

  | BrIf Int
  | Br Int

  | Return
  | End
  deriving (Show, Eq)

data WasmFunction = WasmFunction
  { funcLocals :: [Value]
  , funcBody   :: [Instruction]
  } deriving (Show)

data FuncType = FuncType
  { ftParamCount  :: Int
  , ftResultCount :: Int
  } deriving (Show)

data Export = Export
  { exportName  :: String
  , exportIndex :: Int
  } deriving (Show)


data Global = Global
  { globalType  :: ValType
  , globalMut   :: Bool
  , globalInit  :: Value
  }
  deriving (Show, Eq)

data ValType
  = V_I32
  | V_I64
  | V_F32
  | V_F64
  deriving (Show, Eq)

data Module = Module
  { modTypes      :: [FuncType]
  , modFuncTypes  :: [Int]
  , modGlobals    :: [Global]
  , modExports    :: [Export]
  , modFunctions  :: [WasmFunction]
  } deriving (Show)
