{-# LANGUAGE FlexibleContexts #-}
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

type Index = Int

type Scalar = Float

data Vector = Vector {n::Int, els::[Scalar]}
    deriving (Eq, Show)

type Var = String

--Matrices are m-by-n, represented in row normal form
data Matrix = Matrix {mm::Int, mn::Int, mels::[Vector]}
    deriving (Eq, Show)

{-|
>>> CS (Scalar 1)
CS (Scalar 1)

-}
data Exp
    = CS Scalar -- constant scalar
    | CI Index -- constant index value
    | CV Vector -- constant vector
    | CM Matrix -- constant matrix
    | CVar Var
    | Plus {lhs:: Exp, rhs:: Exp} -- addition
    | Prod {lhs:: Exp, rhs:: Exp} -- multiplication
    | Indx {ex::Exp, idx::Exp} -- indexing into an expression, second argument is the index.
                               -- Indexing into vector yields scalar at that position
                               -- Indexing into matrix yields matrix row at that position
    | Sum {ex::Exp, var::Var, from::Exp, to::Exp}
    deriving (Eq, Show)
