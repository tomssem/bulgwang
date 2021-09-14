module Typer where

import Expressions
    ( Exp(..),
      Matrix(Matrix, mels, mn, mm),
      Var,
      Vector(Vector, els, n) )
import GHC.Exception (errorCallWithCallStackException)
import Data.Functor.Contravariant (Predicate(Predicate))

type TypeMapping = Var -> EType

{-|
The type of the result of an expression
-}
data EType
    = TError String
    | TIndex
    | TScalar
    | TVector Int
    | TMatrix Int Int
    deriving (Eq, Show)

{-|
The type of check functions that operate on the dimensions of two vectors
-}
type VectorBinaryCheckerType = Int -> Int ->EType

{-|
The type of check functions that operate onthe dimensions of two matrices
-}
type MatrixBinaryCheckerType = Int -> Int -> Int -> Int -> EType

{-|
The type of error creation function that takes two operands of a binary operator and returns an object representing the error of performing that operation
on operators of those types
-}
type BinaryErrorType = EType -> EType -> EType

type MatrixDimensionCompatibilityPredicat = Int -> Int -> Int -> Int -> Bool

{-|
Check that binary operators (between homogenous types) behave as defined by the supplied type functions
-}
checkBinaryOperatorType :: TypeMapping -> Exp -> Exp -> EType -> EType -> VectorBinaryCheckerType -> MatrixBinaryCheckerType -> BinaryErrorType -> EType
checkBinaryOperatorType types lhs rhs scalarCase indexCase vectorCase matrixCase errorCase =
    case (lhsType, rhsType) of
        (TScalar, TScalar) -> scalarCase
        (TIndex, TIndex) -> indexCase
        (TVector n1, TVector n2) -> vectorCase n1 n2
        (TMatrix m1 n1, TMatrix m2 n2) -> matrixCase m1 n1 m2 n2
        _ -> errorCase lhsType rhsType
    where lhsType = checkType types lhs
          rhsType = checkType types rhs

checkElementWiseSizeVector :: Int -> Int -> EType -> String -> EType
checkElementWiseSizeVector n1 n2 resultType opName  = if n1 == n2
                                    then resultType
                                    else TError $ "Vectors " ++ show (TVector n1) ++ " and " ++ show (TVector n2) ++ " are incompatible size for " ++ opName

checkMatrixDimensionCompatability :: Int -> Int -> Int -> Int -> MatrixDimensionCompatibilityPredicat -> EType -> String -> EType
checkMatrixDimensionCompatability m1 n1 m2 n2 predicate resultType opName =
    if predicate m1 n1 m2 n2
    then resultType
    else TError $ "Matrices " ++ show (TMatrix m1 n1) ++ " and " ++ show (TMatrix m2 n2) ++ " are incompatible size for " ++ opName

{-|
Currently don't support addition of non-homogenous types
-}
checkPlusType :: TypeMapping -> Exp -> Exp -> EType
checkPlusType types lhs rhs = checkBinaryOperatorType types lhs rhs TScalar TIndex vectorCase matrixCase errorCase
    where vectorCase n1 n2 = checkElementWiseSizeVector n1 n2 (TVector n1) "addition"
          matrixCase m1 n1 m2 n2 = checkMatrixDimensionCompatability m1 n1 m2 n2 (\m1 n1 m2 n2 -> m1 == m2 && n1 == n2) (TMatrix m1 n2) "addition"
          errorCase lhsType rhsType = TError $ "Can not perform addition on types " ++ show lhsType ++ " and " ++ show rhsType

checkProdType :: TypeMapping -> Exp -> Exp -> EType
checkProdType types lhs rhs = checkBinaryOperatorType types lhs rhs TScalar TIndex vectorCase matrixCase errorCase
    where vectorCase n1 n2 = checkElementWiseSizeVector n1 n2 TScalar "product"
          matrixCase m1 n1 m2 n2 = checkMatrixDimensionCompatability m1 n1 m2 n2 (\m1 n1 m2 n2 -> n1 == m2) (TMatrix m1 n2) "product"
          errorCase lhsType rhsType = TError $ "Can not take a product of types " ++ show lhsType ++ " and " ++ show rhsType

isIndexType :: TypeMapping -> Exp -> Bool
isIndexType types ex = checkType types ex == TIndex

isIndexableType :: TypeMapping -> Exp -> Bool
isIndexableType types ex =
    case checkType types ex of
        TVector _ -> True
        TMatrix m n -> True
        _ -> False


checkIndexType :: TypeMapping -> Exp -> Exp -> EType
checkIndexType types ex index =
    if isIndexType types index
        then (case checkType types ex of
                TVector _ -> TScalar
                TMatrix m n -> TVector m
                _ -> TError $ "Can not index into expression of type " ++ show (checkType types ex))
        else TError $ "Can only index using TIndex type, not " ++ show (checkType types index)

checkSumType :: TypeMapping -> Exp -> Var -> Exp -> Exp -> EType
checkSumType types ex var from to =
    case (isIndexableType types ex, isIndexType types from, isIndexType types to) of
        (True, True, True) -> checkIndexType types ex from
        (False, _, _ )     -> TError $ "Can not index into expression of type " ++ show (checkType types ex)
        (True, False, _)   -> TError $ "Can not index with expression of type " ++ show (checkType types from)
        _                  -> TError $ "Can not index with expression of type " ++ show (checkType types to)

checkType :: TypeMapping -> Exp -> EType
checkType types ex =
    case ex of
        CS _ -> TScalar
        CI _ -> TIndex
        CVar var -> types var
        CV Vector {n=n, els=_} -> TVector n
        CM Matrix {mm=m, mn=n, mels=mels} -> TMatrix m n
        Plus {lhs=lhs, rhs=rhs} -> checkPlusType types lhs rhs
        Prod {lhs=lhs, rhs=rhs} -> checkProdType types lhs rhs
        Indx {ex=ex, idx=idx} -> checkIndexType types ex idx
        Sum {ex=ex, var=var, from=from, to=to} -> checkSumType types ex var from to
