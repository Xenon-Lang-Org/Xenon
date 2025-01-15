module ASTSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck()
import Analyzer.SemanticAnalyzer
import Analyzer.IR()
import Parser.Data.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "AST Validation" $ do
    it "should validate simple variable declaration" $ do
      let prog = Program 
            [ VariableDeclaration "x" 
                (PrimitiveType Immutable I32) 
                (Just (ELiteral (IntLiteral 42)))
            ]
      case analyze prog of
        Right result -> do
          finalAst result `shouldBe` prog
        Left err -> expectationFailure $ "Analysis failed: " ++ show err

    it "should detect type mismatches" $ do
      let prog = Program 
            [ VariableDeclaration "x" 
                (PrimitiveType Immutable I32)
                (Just (ELiteral (FloatLiteral 42.0)))
            ]
      analyze prog `shouldBe` Left [TypeMismatch 
        (PrimitiveType Immutable I32) 
        (PrimitiveType Immutable F32)]

    it "should handle function declarations" $ do
      let prog = Program 
            [ FunctionDeclaration "add" 
                [("a", PrimitiveType Immutable I32),
                 ("b", PrimitiveType Immutable I32)]
                (PrimitiveType Immutable I32)
                [ReturnStatement (BinaryOp Add 
                  (Variable "a") 
                  (Variable "b"))]
            ]
      case analyze prog of
        Right result -> finalAst result `shouldBe` prog
        Left err -> expectationFailure $ "Analysis failed: " ++ show err
