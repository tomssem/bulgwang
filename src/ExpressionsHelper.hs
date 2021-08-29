{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

class Expable a b where
    toExp :: a -> Exp b

instance Expable (Exp a) a where
    toExp = id

instance Expable String a where
    toExp = CVar . Var

instance (Num a) => Expable a a where
    toExp = CS . Scalar

instance (Num a) => Expable [a] a where
    toExp [] = error "We don't allow empty vectors"
    toExp x = CV $ Vector (length x) (map Scalar x)

instance (Num a, Eq a) => Expable [[a]] a where
    toExp [] = error "We don't allow empty matrices"
    toExp x = if not allSameLength
                then error "All vectors in matrix must be same length"
                else CM $ Matrix (length x) n (map (Vector n . map Scalar) x)
                where n = length $ head x
                      allSameLength = all (== head x) (tail x)

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
(|+) :: (Expable a c, Expable b c) => a -> b -> Exp c
(|+) x y = Plus (toExp x) (toExp y)

(|!) :: (Expable a c, Expable b Integer) => a -> b -> Exp c
(|!) x y = Indx (toExp x) (toExp y)

sigma :: (Expable a Integer, Expable b Integer, Expable c d) => String -> a -> b -> c -> Exp d
sigma v f t e = Sum (toExp e) (Var v) (toExp f) (toExp t)
