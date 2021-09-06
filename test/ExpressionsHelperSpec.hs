module ExpressionsHelperSpec where

import SpecHelper

spec :: Spec
spec = describe "ExpressionsHelper" $ do
    describe "toExp" $ do
        it "can deal with strings" $ do
            toExp "X" `shouldBe` CVar "X"
        it "can deal with floats" $ do
            toExp (1.0 :: Float) `shouldBe` CS 1.0
        it "can deal with ints" $ do
            toExp (1 :: Int) `shouldBe` CI 1
        it "can deal with vectors of floats" $ do
            toExp [1::Float, 2, 3] `shouldBe` CV (Vector {n = 3, els = [1.0,2.0,3.0]})
        it "can deal with matrices of floats" $ do
            toExp [[1.0::Float, 4.0],[6.7, 3.9]] `shouldBe` CM (Matrix {mm = 2, mn = 2, mels = [Vector {n = 2, els = [1.0,4.0]},Vector {n = 2, els = [6.7,3.9]}]})
    describe "|+" $ do
        it "should construct plus expressions" $ do
            toExp ((1.0 :: Float) |+ (2.0 :: Float)) `shouldBe` Plus {lhs = CS 1.0, rhs = CS 2.0}
    describe "|." $ do
        it "should construct product expressions" $ do
            toExp ((1.0 :: Float) |. (2.0 :: Float)) `shouldBe` Prod {lhs = CS 1.0, rhs = CS 2.0}
    describe "||" $ do
        it "sould construct index expressions" $ do
            toExp ([1.0::Float, 2.0::Float] |!! (1 :: Int)) `shouldBe` Indx {ex = CV (Vector {n = 2, els = [1.0,2.0]}), idx = CI 1}
    describe "sigma" $ do
        it "should construct summ expressions" $ do
            toExp (sigma "X" (1 :: Int) (10 :: Int) ("X" |+ (2.0 :: Float))) `shouldBe` Sum {ex = Plus {lhs = CVar "X", rhs = CS 2.0}, var = "X", from = CI 1, to = CI 10}

main :: IO ()
main = hspec spec