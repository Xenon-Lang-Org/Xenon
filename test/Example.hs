module Example (main, spec) where

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Example" $ do
        it "should be true" $ do
            True `shouldBe` True