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

import Expressions ( Exp(Plus, Indx, CVar, CS, CV, CM, Sum), Var( Var ), Scalar (Scalar), Vector (Vector), Matrix (Matrix))
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
    toExp = CVar . Var

{-|
Implicitly converts all nums to floats
-}
instance Expable Float where
    toExp = CS . Scalar

instance Expable [Float] where
    toExp [] = error "We don't allow empty vectors"
    toExp x = CV $ Vector (length x) (map Scalar x)

instance Expable [[Float]]where
    toExp [] = error "Dimension m of matrix is 0"
    toExp x
      | n == 0 = error "Dimension n of matrix is 0"
      | not allSameLength = error "All vectors in matrix must be same length"
      | otherwise = CM $ Matrix (length x) n (map toVector x)
      where
          n = length $ head x
          allSameLength = all ((== n) . length) (tail x)
          toVector = Vector n . map Scalar

{-|
Allows us to easily add together haskell primatives.
>>> toExp 1
WAS NOW CS (Scalar 1)
NOW Could not deduce (Expable a0 b)
NOW from the context: (Expable a b, Num a)
NOW   bound by the inferred type for ‘it’:
NOW              forall a b. (Expable a b, Num a) => Exp b
NOW   at /home/tom/haskell/bulgwang/src/ExpressionsHelper.hs:49:2-8
NOW The type variable ‘a0’ is ambiguous


-}
(|+) :: (Expable a, Expable b) => a -> b -> Exp
(|+) x y = Plus (toExp x) (toExp y)

(|!) :: (Expable a, Expable b) => a -> b -> Exp
(|!) x y = Indx (toExp x) (toExp y)

sigma :: (Expable a, Expable b, Expable c) => String -> a -> b -> c -> Exp
sigma v f t e = Sum (toExp e) (Var v) (toExp f) (toExp t)
