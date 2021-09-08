module Typer where

import Expressions
    ( Exp(..),
      Matrix(Matrix, mels, mn, mm),
      Var,
      Vector(Vector, els, n) )
import GHC.Exception (errorCallWithCallStackException)

type TypeMapping = Var -> EType

{-|
The type of the result of an expression
>>> :t show
show :: Show a => a -> String
-}
data EType
    = TError String
    | TIndex
    | TScalar
    | TVector Int
    | TMatrix Int Int
    deriving (Eq, Show)

checkBinaryOperatorType :: TypeMapping -> Exp -> Exp -> EType -> (Int -> Int ->EType) -> (Int -> Int -> Int -> Int -> EType) -> (EType -> EType -> EType) -> EType
checkBinaryOperatorType types lhs rhs scalarCase vectorCase matrixCase errorCase =
    case (lhsType, rhsType) of
        (TScalar, TScalar) -> scalarCase
        (TVector n1, TVector n2) -> vectorCase n1 n2
        (TMatrix m1 n1, TMatrix m2 n2) -> matrixCase m1 n1 m2 n2
        _ -> errorCase lhsType rhsType
    where lhsType = checkType types lhs
          rhsType = checkType types rhs

checkElementWiseSizeVector :: Exp -> Exp -> Int -> Int -> String -> EType
checkElementWiseSizeVector lhs rhs n1 n2 opName  = if n1 == n2
                                    then TVector n1
                                    else TError $ "Vectors " ++ show lhs ++ " and " ++ show rhs ++ " are incompatible size for " ++ opName

checkPlusType :: TypeMapping -> Exp -> Exp -> EType
checkPlusType types lhs rhs = checkBinaryOperatorType types lhs rhs scalarCase vectorCase matrixCase errorCase
    where scalarCase = TScalar
          vectorCase n1 n2 = checkElementWiseSizeVector lhs rhs n1 n2 "addition"
          matrixCase m1 n1 m2 n2 = if m1 == m2 && n1 == n2
                                    then TMatrix m1 n1
                                    else TError $ "Matrices " ++ show lhs ++ " and " ++ show rhs ++ " are incompatible size for addition"
          errorCase lhsType rhsType = TError $ "Cannot perform addition on types " ++ show lhsType ++ " and " ++ show rhsType

checkProdType :: TypeMapping -> Exp -> Exp -> EType
checkProdType types lhs rhs = checkBinaryOperatorType types lhs rhs scalarCase vectorCase matrixCase errorCase
    where scalarCase = TScalar
          vectorCase n1 n2 = checkElementWiseSizeVector lhs rhs n1 n2 "product"
          matrixCase m1 n1 m2 n2 = if n1 == m1
                                    then TMatrix m1 n2
                                    else TError $ "Matrices " ++ show lhs ++ " and " ++ show rhs ++ " are incompatible shape for product"
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
                TMatrix m n -> TVector n
                _ -> TError $ "Can not index into expression of type " ++ show (checkType types ex))
        else TError $ "Can only index using index type, not" ++ show (checkType types index)

checkSumType :: TypeMapping -> Exp -> Var -> Exp -> Exp -> EType
checkSumType types ex var from to =
    case (isIndexableType types ex, isIndexType types from, isIndexType types to) of
        (True, True, True) -> checkIndexType types ex from
        (False, _, _ )     -> TError $ "Cannot index into expression of type " ++ show (checkType types ex)
        (True, False, _)   -> TError $ "Cannot index with expression of type " ++ show (checkType types from)
        _                  -> TError $ "Cannot index with expression of type " ++ show (checkType types to)

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
