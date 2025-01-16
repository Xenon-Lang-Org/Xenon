module VM.Data.Types
  ( Value(..)
  , Instruction(..)
  , FunctionIndex
  ) where

data Value
  = I32 Int
  | I64 Int
  | F32 Float
  | F64 Double
  deriving (Show, Eq)

data Instruction
  = I32Const Int
  | I32Add
  | I32Sub
  | I32Mul
  | End
  deriving (Show, Eq)

type FunctionIndex = Int
