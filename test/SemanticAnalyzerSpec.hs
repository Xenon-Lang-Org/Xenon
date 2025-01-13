module SemanticAnalyzerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck()
import Analyzer.SemanticAnalyzer
import Analyzer.IR()
import Parser.Data.Ast
import Data.Either (isRight, isLeft)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Semantic Analyzer" $ do
    describe "Variable Analysis" $ do
      it "should accept valid variable declarations" $ do
        let prog = Program 
              [ VariableDeclaration "x" 
                  (PrimitiveType Immutable I32) 
                  (Just (ELiteral (IntLiteral 42)))
              ]
        analyze prog `shouldBe` Right prog

      it "should detect type mismatches in declarations" $ do
        let prog = Program 
              [ VariableDeclaration "x" 
                  (PrimitiveType Immutable I32)
                  (Just (ELiteral (FloatLiteral 42.0)))
              ]
        analyze prog `shouldBe` Left [TypeMismatch 
          (PrimitiveType Immutable I32) 
          (PrimitiveType Immutable F32)]

      it "should handle multiple variable declarations" $ do
        let prog = Program 
              [ VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 1)))
              , VariableDeclaration "y" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 2)))
              ]
        analyze prog `shouldSatisfy` isRight

      it "should detect redeclaration of variables" $ do
        let prog = Program 
              [ VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 1)))
              , VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 2)))
              ]
        analyze prog `shouldSatisfy` isLeft
