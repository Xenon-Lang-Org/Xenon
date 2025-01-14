module SemanticAnalyzerSpec (main, spec) where

import Test.Hspec
import Analyzer.SemanticAnalyzer
import Parser.Data.Ast
import Data.Either (isRight, isLeft)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Semantic Analysis" $ do
    it "should validate variable declarations" $ do
      let prog = Program 
            [ VariableDeclaration "x" 
                (PrimitiveType Immutable I32) 
                (Just (ELiteral (IntLiteral 42)))
            ]
      analyze prog `shouldSatisfy` isRight

    it "should detect type errors" $ do
      let prog = Program 
            [ VariableDeclaration "x" 
                (PrimitiveType Immutable I32)
                (Just (ELiteral (FloatLiteral 42.0)))
            ]
      analyze prog `shouldSatisfy` isLeft