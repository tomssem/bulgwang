module TyperSpec where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import SpecHelper
import Typer ( checkType, EType(TScalar, TIndex, TVector, TMatrix), EType)

emptyTypes = (!) (Map.empty :: (Map String EType))


expectVarTypes :: EType -> Expectation 
expectVarTypes t = checkType (\x -> if x == "X" then t else error "") (CVar "X") `shouldBe` t

{-|
>>> [[1::Float, 2, 3], [3, 4, 5]] |!! (1::Int)
Indx {ex = CM (Matrix {mm = 2, mn = 3, mels = [Vector {n = 3, els = [1.0,2.0,3.0]},Vector {n = 3, els = [3.0,4.0,5.0]}]}), idx = CI 1}
-}
spec :: Spec
spec = describe "Typer" $ do
    describe "checkTypes" $ do
        describe "raw types" $ do
            it "can deal with scalar types" $ do 
                checkType emptyTypes (CS (1::Float)) `shouldBe` TScalar
            it "can deal with index types" $ do
                checkType emptyTypes (CI (1::Int)) `shouldBe` TIndex
        describe "variables" $ do
            it "can deal with var types of type scalar" $ do
                expectVarTypes TScalar
            it "can deal with var types of type index" $ do
                expectVarTypes TIndex
            it "can deal with var types of type vector" $ do
                expectVarTypes (TVector 3)
            it "can deal with var types of type matrix" $ do
                expectVarTypes (TMatrix  6 3)
        describe "addition" $ do 
            it "can type pluses of scalars" $ do
                checkType emptyTypes ((1.0 :: Float) |+ (2.0 :: Float)) `shouldBe` TScalar 
            it "can type pluses of Index" $ do
                checkType emptyTypes ((1::Int) |+ (2::Int)) `shouldBe` TIndex
            it "can type pluses of vectors length 2" $ do
                checkType emptyTypes ([1::Float, 2] |+ [3::Float, 4]) `shouldBe` TVector 2 
            it "can type pluses of vectors length 1" $ do
                checkType emptyTypes ([1::Float] |+ [2::Float]) `shouldBe` TVector 1
            it "can type pluses of matrices" $ do
                checkType emptyTypes ([[1::Float, 2, 3], [4, 5, 6]] |+ [[10::Float, 11, 12], [13, 14, 15]]) `shouldBe` TMatrix 2 3
        describe "multiplication" $ do
            it "can multiply scalars" $ do
                 checkType emptyTypes ((1.0 :: Float) |. (2.0 :: Float)) `shouldBe` TScalar
            it "can multiply Indexes" $ do
                checkType emptyTypes ((1::Int) |. (2::Int)) `shouldBe` TIndex
            it "can multiply Vectors" $ do
                checkType emptyTypes ([1::Float, 2] |. [3::Float, 4]) `shouldBe` TScalar
            it "can multiple matrices" $ do
                checkType emptyTypes ([[1::Float, 2, 3], [3, 4, 5]] |. [[7::Float, 8], [9, 10], [11, 12]]) `shouldBe` TMatrix 2 2
        describe "index expressions" $ do
            it "vectors" $ do
                checkType emptyTypes ([1::Float, 2, 3] |!! (0::Int)) `shouldBe` TScalar
            it "matrices" $ do
                checkType emptyTypes ([[1::Float, 2, 3], [3, 4, 5]] |!! (0::Int)) `shouldBe` TVector 2



main :: IO ()
main = hspec spec
