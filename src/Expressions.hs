{-# LANGUAGE FlexibleContexts #-}
module Expressions where

type Index = Exp Integer

newtype Scalar a = Scalar a
    deriving (Eq, Show)

newtype Vector a = Vector [Scalar a]
    deriving (Eq, Show)

newtype Var = Var String
    deriving (Eq, Show)

--Matrices are represented in column normal form
{-|
>>> [8]
[8]
-}
newtype Matrix a = M [Vector a]
    deriving (Eq, Show)

{-|
>>> CS (Scalar 1)
CS (Scalar 1)

-}
data Exp a
    = CS (Scalar a) -- constant scalar
    | CV (Vector a) -- constant vector
    | CM (Matrix a) -- constant matrix
    | Plus {lhs:: Exp a, rhs:: Exp a} -- addition
    | Mult {lhs:: Exp a, rhs:: Exp a} -- multiplication
    | Indx {exp::Exp a, idx::Index} -- indexing into an expression, second argument is the index
    | Sum {exp::Exp a, var::Var, from::Index, to::Index}
    deriving (Eq, Show)
