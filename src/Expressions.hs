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
data Exp
    = CS (Scalar Float) -- constant scalar
    | CV (Vector Float) -- constant vector
    | CM (Matrix Float) -- constant matrix
    | CVar Var
    | Plus {lhs:: Exp, rhs:: Exp} -- addition
    | Mult {lhs:: Exp, rhs:: Exp} -- multiplication
    | Indx {exp::Exp, idx::Exp} -- indexing into an expression, second argument is the index
    | Sum {exp::Exp, var::Var, from::Exp, to::Exp}
    deriving (Eq, Show)
