{-# LANGUAGE ScopedTypeVariables #-}

module LexerQuickCheckSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Parser.System.Lexer
import Data.Void
import qualified Text.Megaparsec as MP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer.tokens with QuickCheck" $ do
    it "should parse identifiers with valid names" $ property $
      \(ValidIdentifier name) -> do
        let result = runLexer name
        case result of
          Right [Token _ (TIdent parsedName), Token _ TEOF] -> parsedName `shouldBe` name
          _ -> expectationFailure $ "Unexpected result: " ++ show result

    it "should parse valid integer literals" $ property $
      \(NonNegative n :: NonNegative Integer) -> do
        let input = show n
        let result = runLexer input
        case result of
          Right [Token _ (TIntLit parsedNum), Token _ TEOF] -> parsedNum `shouldBe` n
          _ -> expectationFailure $ "Unexpected result: " ++ show result

    it "should parse valid float literals" $ property $
      \(Positive f :: Positive Double) -> do
        let input = show f
        let result = runLexer input
        case result of
          Right [Token _ (TFloatLit parsedNum), Token _ TEOF] -> parsedNum `shouldBe` f
          _ -> expectationFailure $ "Unexpected result: " ++ show result

    it "should parse combinations of identifiers and literals" $ property $
      \(ValidIdentifier name, NonNegative n :: NonNegative Integer) -> do
        let input = name ++ " = " ++ show n ++ ";"
        let result = runLexer input
        case result of
          Right [ Token _ (TIdent parsedName),
                  Token _ TEqSign,
                  Token _ (TIntLit parsedNum),
                  Token _ TSemicolon,
                  Token _ TEOF ] -> do
            parsedName `shouldBe` name
            parsedNum `shouldBe` n
          _ -> expectationFailure $ "Unexpected result: " ++ show result

-- Helper function to run the lexer
runLexer :: String -> Either (MP.ParseErrorBundle String Void) [Token]
runLexer = MP.runParser tokens "<quickcheck-test>"

-- Arbitrary generator for valid identifiers
newtype ValidIdentifier = ValidIdentifier String
  deriving (Show, Eq)

instance Arbitrary ValidIdentifier where
  arbitrary = do
    firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
    rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
    return $ ValidIdentifier (firstChar : rest)
