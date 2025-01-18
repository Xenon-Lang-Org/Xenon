module VM.Data.VMTypes
  ( VM(..)
  , Frame(..)
  , Label(..)
  ) where

import VM.Data.AST (Value(..), Module(..))

data Label = Label
  { labelType :: String
  , labelPC   :: Int
  , labelCond :: Maybe Int
  } deriving (Show)

data Frame = Frame
  { frFuncIndex :: Int
  , frLocals    :: [Value]
  , frPC        :: Int
  , frLabels    :: [Label]
  } deriving (Show)

data VM = VM
  { vmModule     :: Module
  , vmFrames     :: [Frame]
  , operandStack :: [Value]
  , vmGlobals    :: [Value]
  , vmTable      :: [Int]
  } deriving (Show)
