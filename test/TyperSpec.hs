module TyperSpec where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import SpecHelper
import Typer ( checkType, EType(TScalar, TIndex, TVector, TMatrix), EType)

emptyTypes = (!) (Map.empty :: (Map String EType))


expectVarTypes :: EType -> Expectation 
expectVarTypes t = checkType (\x -> if x == "X" then t else error "") (CVar "X") `shouldBe` t

{-|
>>> :t shouldBe
WAS checkType :: TypeMapping -> Exp -> EType
NOW shouldBe :: (Show a, Eq a) => a -> a -> Expectation
-}
spec :: Spec
spec = describe "Typer" $ do
    describe "checkTypes" $ do
        it "can deal with scalar types" $ do 
            checkType emptyTypes (CS (1::Float)) `shouldBe` TScalar
        it "can deal with index types" $ do
            checkType emptyTypes (CI (1::Int)) `shouldBe` TIndex
        it "can deal with var types of type scalar" $ do
            expectVarTypes TScalar
        it "can deal with var types of type index" $ do
            expectVarTypes TIndex
        it "can deal with var types of type vector" $ do
            expectVarTypes (TVector 3)
        it "can deal with var types of type matrix" $ do
            expectVarTypes (TMatrix  6 3)
        it "can type pluses of scalars" $ do
            checkType emptyTypes ((1.0 :: Float) |+ (2.0 :: Float)) `shouldBe` TScalar 
        it "can type pluses of Index" $ do
            checkType emptyTypes ((1::Int) |+ (2::Int)) `shouldBe` TIndex
        it "can pluses of vectors" $ do
            checkType emptyTypes [1::Float, 2] |+ [3::Float, 4] `shouldBe` TVector 2 

main :: IO ()
main = hspec spec
