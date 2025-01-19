module Analyzer.CheckValue
  ( isInvalidRootStatement
  ) where

import Parser.Data.Ast (Statement(..))

------------------------------------------------------------------------------
-- | check statement in root
-- Authorised in root: VariableDeclaration, FunctionDeclaration, TypeDeclaration
-- Not authorised in root: WhileLoop, If, ReturnStatement, StandaloneFunctionCall, VariableReAssignment
-------------------------------------------------------------------------------

isInvalidRootStatement :: Statement -> Bool
isInvalidRootStatement stmt = case stmt of
    VariableDeclaration {} -> False
    FunctionDeclaration {} -> False
    TypeDeclaration {} -> False
    WhileLoop {} -> True
    If {} -> True
    ReturnStatement {} -> True
    StandaloneFunctionCall {} -> True
    VariableReAssignment {} -> True
