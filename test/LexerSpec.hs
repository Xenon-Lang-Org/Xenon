module LexerSpec (main, spec) where

import Data.Void
import Parser.System.Lexer
import Test.Hspec
import qualified Text.Megaparsec as MP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lexer.tokens" $ do
    it "should correctly tokenize simple keywords" $ do
      runLexer "let"
        `shouldBe` Right
          [ Token (pos 1 1) TLet,
            Token (pos 1 4) TEOF
          ]

      runLexer "fn"
        `shouldBe` Right
          [ Token (pos 1 1) TFn,
            Token (pos 1 3) TEOF
          ]

    it "should tokenize integers and floats" $ do
      runLexer "42"
        `shouldBe` Right
          [ Token (pos 1 1) (TIntLit 42),
            Token (pos 1 3) TEOF
          ]

      runLexer "-3.14"
        `shouldBe` Right
          [ Token (pos 1 1) (TFloatLit (-3.14)),
            Token (pos 1 6) TEOF
          ]

    it "should tokenize identifiers" $ do
      runLexer "x"
        `shouldBe` Right
          [ Token (pos 1 1) (TIdent "x"),
            Token (pos 1 2) TEOF
          ]

      runLexer "hello_world"
        `shouldBe` Right
          [ Token (pos 1 1) (TIdent "hello_world"),
            Token (pos 1 12) TEOF
          ]

    it "should tokenize symbols" $ do
      runLexer ":"
        `shouldBe` Right
          [ Token (pos 1 1) TColon,
            Token (pos 1 2) TEOF
          ]

      runLexer ";"
        `shouldBe` Right
          [ Token (pos 1 1) TSemicolon,
            Token (pos 1 2) TEOF
          ]

      runLexer "->"
        `shouldBe` Right
          [ Token (pos 1 1) TArrow,
            Token (pos 1 3) TEOF
          ]

    it "should skip comments and whitespace" $ do
      runLexer "let x = 42; // this is a comment"
        `shouldBe` Right
          [ Token (pos 1 1) TLet,
            Token (pos 1 5) (TIdent "x"),
            Token (pos 1 7) TEqSign,
            Token (pos 1 9) (TIntLit 42),
            Token (pos 1 11) TSemicolon,
            Token (pos 1 33) TEOF
          ]

      runLexer "/* block comment */let x = 42;"
        `shouldBe` Right
          [ Token (pos 1 20) TLet,
            Token (pos 1 24) (TIdent "x"),
            Token (pos 1 26) TEqSign,
            Token (pos 1 28) (TIntLit 42),
            Token (pos 1 30) TSemicolon,
            Token (pos 1 31) TEOF
          ]

    it "should parse very long numbers" $ do
      runLexer "123456789012345678901234567890"
        `shouldBe` Right
          [ Token (pos 1 1) (TIntLit 123456789012345678901234567890),
            Token (pos 1 31) TEOF
          ]

      runLexer "123456789012345678901234567890.123456789012345678901234567890"
        `shouldBe` Right
          [ Token (pos 1 1) (TFloatLit 123456789012345678901234567890.123456789012345678901234567890),
            Token (pos 1 62) TEOF
          ]

    it "should handle normal expressions" $ do
      runLexer "let x: i32 = 42;"
        `shouldBe` Right
          [ Token (pos 1 1) TLet,
            Token (pos 1 5) (TIdent "x"),
            Token (pos 1 6) TColon,
            Token (pos 1 8) (TIdent "i32"),
            Token (pos 1 12) TEqSign,
            Token (pos 1 14) (TIntLit 42),
            Token (pos 1 16) TSemicolon,
            Token (pos 1 17) TEOF
          ]

      runLexer "fn add(a: i32, b: i32) -> i32 { return a + b; }"
        `shouldBe` Right
          [ Token (pos 1 1) TFn,
            Token (pos 1 4) (TIdent "add"),
            Token (pos 1 7) TOpenParen,
            Token (pos 1 8) (TIdent "a"),
            Token (pos 1 9) TColon,
            Token (pos 1 11) (TIdent "i32"),
            Token (pos 1 14) TComma,
            Token (pos 1 16) (TIdent "b"),
            Token (pos 1 17) TColon,
            Token (pos 1 19) (TIdent "i32"),
            Token (pos 1 22) TCloseParen,
            Token (pos 1 24) TArrow,
            Token (pos 1 27) (TIdent "i32"),
            Token (pos 1 31) TOpenBrace,
            Token (pos 1 33) TReturn,
            Token (pos 1 40) (TIdent "a"),
            Token (pos 1 42) TPlus,
            Token (pos 1 44) (TIdent "b"),
            Token (pos 1 45) TSemicolon,
            Token (pos 1 47) TCloseBrace,
            Token (pos 1 48) TEOF
          ]

    it "should handle invalid input gracefully" $ do
      case runLexer "#" of
        Left err -> MP.errorBundlePretty err `shouldContain` "unexpected '#'"
        Right _ -> expectationFailure "Expected failure, but succeeded!"

-- Helper function to run the lexer and simplify error handling
runLexer :: String -> Either (MP.ParseErrorBundle String Void) [Token]
runLexer = MP.runParser tokens "<test>"

-- Helper to construct SourcePos for tokens
pos :: Int -> Int -> MP.SourcePos
pos line col = MP.SourcePos "<test>" (MP.mkPos line) (MP.mkPos col)
