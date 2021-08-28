module ExpressionsSpec where

import SpecHelper

spec :: Spec
spec = describe "something" $ do
    it "can do anything" $ do
        1 `shouldBe` 1

main :: IO ()
main = hspec spec