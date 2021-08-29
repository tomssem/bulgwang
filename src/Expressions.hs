{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}
{-|
Module      : Expressions
Description : Base definitions of Expression syntax
Copyright   : (c) Tom Nicholson 2021
License     : MIT
Maintainer  : tfwnicholson@gmail.com
Stability   : experimental

Provides base defintions of expressions which can be used to build up more complicated expressions
-}
module Expressions where

type Index = Exp Integer

newtype (Num a) => Scalar a = Scalar a
    deriving (Eq, Show)

data Vector a = Vector {n::Int, els::[Scalar a]}
    deriving (Eq, Show)

newtype Var = Var String
    deriving (Eq, Show)

--Matrices are mxn represented in column normal form
data Matrix a = Matrix {mm::Int, mn::Int, mels::[Vector a]}
    deriving (Eq, Show)

{-|
>>> CS (Scalar 1)
CS (Scalar 1)

-}
data Exp a
    = CS (Scalar a) -- constant scalar
    | CV (Vector a) -- constant vector
    | CM (Matrix a) -- constant matrix
    | CVar Var
    | Plus {lhs:: Exp a, rhs:: Exp a} -- addition
    | Mult {lhs:: Exp a, rhs:: Exp a} -- multiplication
    | Indx {exp::Exp a, idx::Index} -- indexing into an expression, second argument is the index
    | Sum {exp::Exp a, var::Var, from::Index, to::Index}
    deriving (Eq, Show)
