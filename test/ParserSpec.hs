{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (main, spec) where

import Test.QuickCheck hiding (Negative)
import Data.Void
import Parser.Data.Ast
import qualified Parser.System.Lexer as Lexer
import Parser.System.Parser
import Test.Hspec
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser tests" $ do
    it "parses variable declarations correctly" $ do
      let input = "let x: i32 = 42;"
      runParserTest input `shouldBe` Right (Program [VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (ELiteral (IntLiteral 42)))])

    it "parses negative integer literals correctly" $ do
      let input = "let x: i32 = -42;"
      runParserTest input `shouldBe` Right (Program [VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (UnaryOp Negative (ELiteral (IntLiteral 42))))])

    it "parses basic arithmetic expressions correctly" $ do
      let addition = "let x: i32 = 5 + 3 + 2;"
      runParserTest addition `shouldSatisfy` isRight

      let subtraction = "let x: i32 = 5 - 3 - 2;"
      runParserTest subtraction `shouldSatisfy` isRight

      let multiplication = "let x: i32 = 5 * 3 * 2;"
      runParserTest multiplication `shouldSatisfy` isRight

      let division = "let x: f32 = 5 / 3 / 2;"
      runParserTest division `shouldSatisfy` isRight

      let modulo = "let x: i32 = 5 % 3 % 2;"
      runParserTest modulo `shouldSatisfy` isRight

    it "parses mixed arithmetic expressions correctly" $ do
      let input1 = "let x: i32 = 5 + 3 * 2;"
      runParserTest input1
        `shouldBe` Right
          ( Program
              [ VariableDeclaration
                  "x"
                  (PrimitiveType Immutable I32)
                  ( Just
                      (BinaryOp Add (ELiteral (IntLiteral 5)) (BinaryOp Mul (ELiteral (IntLiteral 3)) (ELiteral (IntLiteral 2))))
                  )
              ]
          )

      let input2 = "let x: i32 = 5 * 3 + 2;"
      runParserTest input2
        `shouldBe` Right
          ( Program
              [ VariableDeclaration
                  "x"
                  (PrimitiveType Immutable I32)
                  ( Just
                      (BinaryOp Add (BinaryOp Mul (ELiteral (IntLiteral 5)) (ELiteral (IntLiteral 3))) (ELiteral (IntLiteral 2)))
                  )
              ]
          )

      let input3 = "let x: i32 = 5 * (3 + 2);"
      runParserTest input3 `shouldSatisfy` isRight

      let input4 = "let x: i32 = (5 - 2) * 3;"
      runParserTest input4 `shouldSatisfy` isRight

    it "parses complex operations using negative literals correctly" $ do
      let input = "let x: i32 = 5 + -3 * 2;"
      runParserTest input
        `shouldBe` Right
          ( Program
              [ VariableDeclaration
                  "x"
                  (PrimitiveType Immutable I32)
                  ( Just
                      ( BinaryOp
                          Add
                          (ELiteral (IntLiteral 5))
                          (BinaryOp Mul (UnaryOp Negative (ELiteral (IntLiteral 3))) (ELiteral (IntLiteral 2)))
                      )
                  )
              ]
          )

      let expected_output = Right (Program [VariableDeclaration "x" (PrimitiveType Immutable I32) (Just (BinaryOp Sub (ELiteral (IntLiteral 3)) (UnaryOp Negative (ELiteral (IntLiteral 2)))))])
      runParserTest "let x: i32 = 3 -- 2;" `shouldBe` expected_output
      runParserTest "let x: i32 = 3 - -2;" `shouldBe` expected_output

    it "parses function declarations correctly" $ do
      let input = "fn add(a: i32, b: i32) -> i32 { return a + b; }"
      runParserTest input `shouldSatisfy` isRight

    it "parses while loops correctly" $ do
      let input = "while (x < 10) { x = x + 1; }"
      runParserTest input `shouldSatisfy` isRight

    it "parses if statements with else correctly" $ do
      let input = "if (x == 0) { x = 1; } else { x = 0; }"
      runParserTest input `shouldSatisfy` isRight

    it "parses struct type declarations correctly" $ do
      let input = "type Point = { x: i32, y: i32 };"
      runParserTest input `shouldBe` Right (Program [TypeDeclaration "Point" (StructType Immutable (Struct [("x", PrimitiveType Immutable I32), ("y", PrimitiveType Immutable I32)]))])

    it "parses array type declarations correctly" $ do
      let input = "type IntArray = [10: i32];"
      runParserTest input `shouldSatisfy` isRight

    it "parses enum type declarations correctly" $ do
      let input = "type Color = <Red, Green, Blue>;"
      runParserTest input `shouldSatisfy` isRight

    it "handles reassignment correctly" $ do
      let input = "let x: i32 = 0; x = 5;"
      runParserTest input `shouldSatisfy` isRight

  describe "QuickCheck property tests" $ do
    it "always parses valid variable declarations" $ property $
      forAll genVariableDeclaration $ \(name, value) ->
        let input = "let " ++ name ++ ": i32 = " ++ show value ++ ";"
        in isRight (runParserTest input) -- if negative will output (UnaryOp Negative (ELiteral (IntLiteral ...)))

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

runParserTest :: String -> Either String Program
runParserTest input = case runLexer input of
  Left err -> Left $ "Lexer error: " ++ errorBundlePretty err
  Right tokenss -> case parse parseProgram "test" (Lexer.TokenStream tokenss input) of
    Left err -> Left $ "Parser error: " ++ errorBundlePretty err
    Right result -> Right result

runLexer :: String -> Either (ParseErrorBundle String Void) [Lexer.Token]
runLexer = parse Lexer.tokens "test"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-------------------------------------------------------------------------------
-- Arbitrary Generators for QuickCheck
-------------------------------------------------------------------------------

genVariableName :: Gen String
genVariableName = (:) <$> elements ['a'..'z'] <*> listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_'])

genIntValue :: Gen Int
genIntValue = arbitrary --`suchThat` (>= 0)

genVariableDeclaration :: Gen (String, Int)
genVariableDeclaration = (,) <$> genVariableName <*> genIntValue
