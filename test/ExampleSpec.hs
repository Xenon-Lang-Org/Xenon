module ExampleSpec (main, spec) where

import Test.Hspec
import Utils.System.Math (safeDiv)
import Utils.Data.Result (Result(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Example" $ do
        it "should be true" $ do
            True `shouldBe` True

        it "oui" $ do
            safeDiv 4 2 `shouldBe` Ok (2 :: Int)
            safeDiv 4 (0 :: Int) `shouldBe` Err ("Division by zero" :: String)
            (1 :: Int) `shouldBe` (1 :: Int)
