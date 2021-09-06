{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : ExpressionsHelper
Description : Syntactic sugar for expressions
Copyright   : (c) Tom Nicholson 2021
License     : MIT
Maintainer  : tfwnicholson@gmail.com
Stability   : experimental

Gives us some nicer syntax for building up Bulgwang expressions
-}

-- For equations 2.1 would like to be able to do:
-- "B0" |+ sigma "j" 1 "p" (("X" |! "j") b* ("B" |! "j")

module ExpressionsHelper where

import Expressions ( Exp(Plus, Prod, Indx, CVar, CS, CI, CV, CM, Sum), Var, Scalar, Vector (Vector), Matrix (Matrix), Index)
import Control.Exception (assert)
import Data.Function ( on )

class Expable a where
    toExp :: a -> Exp

{-|
>>> toExp (CS (Scalar 1))
Could not deduce (Expable (Exp a0) b)
from the context: (Expable (Exp a) b, Num a)
  bound by the inferred type for ‘it’:
             forall a b. (Expable (Exp a) b, Num a) => Exp b
  at /home/tom/haskell/bulgwang/src/ExpressionsHelper.hs:27:2-22
The type variable ‘a0’ is ambiguous
-}
instance Expable Exp where
    toExp = id

{-|
>>> toExp "hello"
CVar (Var "hello")
-}
instance Expable String where
    toExp = CVar

{-|
Floats become scalars
-}
instance Expable Float where
    toExp = CS

{-|
Ints become indices
-}
instance Expable Int where
    toExp = CI

{-|
>>> toExp [1.0::Float, 2, 3]
CV (Vector {n = 3, els = [1.0,2.0,3.0]})
>>> toExp ([] :: [Float])
We don't allow empty vectors
-}
instance Expable [Float] where
    toExp [] = error "We don't allow empty vectors"
    toExp x = CV $ Vector (length x) x

{-|
>>> toExp [[1.0::Float, 4.0],[6.7, 3.9]]
CM (Matrix {mm = 2, mn = 2, mels = [Vector {n = 2, els = [1.0,4.0]},Vector {n = 2, els = [6.7,3.9]}]})
-}
instance Expable [[Float]]where
    toExp [] = error "Dimension m of matrix is 0"
    toExp x
      | n == 0 = error "Dimension n of matrix is 0"
      | not allSameLength = error "All vectors in matrix must be same length"
      | otherwise = CM $ Matrix (length x) n (map toVector x)
      where
          n = length $ head x
          allSameLength = all ((== n) . length) (tail x)
          toVector = Vector n

{-|
Allows us to easily add together haskell primatives.
>>> toExp "X"
CVar (Var "X")

>>> toExp (1.0 :: Float)
CS 1.0

>>> toExp $ (1.0 :: Float) |+ (2.0 :: Float)
Plus {lhs = CS 1.0, rhs = CS 2.0}
-}
(|+) :: (Expable a, Expable b) => a -> b -> Exp
(|+) x y = Plus (toExp x) (toExp y)

{-|
>>> toExp $ (1.0 :: Float) |. (2.0 :: Float)
Prod {lhs = CS 1.0, rhs = CS 2.0}
-}
(|.) :: (Expable a, Expable b) => a -> b -> Exp
(|.) x y = Prod (toExp x) (toExp y)

{-|
>>> toExp $ [1.0::Float, 2.0::Float] |!! (1 :: Int)
Indx {ex = CV (Vector {n = 2, els = [1.0,2.0]}), idx = CI 1}
-}
(|!!) :: (Expable a, Expable b) => a -> b -> Exp
(|!!) x y = Indx (toExp x) (toExp y)

{-|
>>> toExp $ sigma "X" (1 :: Int) (10 :: Int) ("X" |+ (2.0 :: Float)) 
Sum {ex = Plus {lhs = CVar "X", rhs = CS 2.0}, var = "X", from = CI 1, to = CI 10}
-}
sigma :: (Expable a, Expable b, Expable c) => String -> a -> b -> c -> Exp
sigma v f t e = Sum (toExp e) v (toExp f) (toExp t)
