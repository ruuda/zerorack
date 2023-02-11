-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Expr
  ( Expr (..)
  , ExprExt (..)
  , VarNum (..)
  , RegNum (..)
  , Increment (..)
  )
where

-- Variables are numbered. To have some distinction between variables in the
-- input expressions and in the output constraints, in the compilation phase,
-- we call the variables in the constraints "registers" instead of "variables".
newtype VarNum = VarNum Int deriving (Eq, Ord)
newtype RegNum = RegNum Int deriving (Eq, Ord)

instance Show VarNum where
  show (VarNum i) = "x" <> (show i)

instance Show RegNum where
  show (RegNum 0) = "1" -- Register 0 is always implicitly 1.
  show (RegNum i) = "r" <> (show i)

class Increment a where
  inc :: a -> a

instance Increment VarNum where
  inc (VarNum i) = VarNum (i + 1)

instance Increment RegNum where
  inc (RegNum i) = RegNum (i + 1)

-- A simple expression, one that we can express as an arithmetic circuit.
data Expr
  = Const Integer
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Var VarNum
  deriving (Eq, Ord, Show)

instance Num Expr where
  x + y = Add x y
  x - y = Sub x y
  x * y = Mul x y
  negate x = Sub (Const 0) x
  abs _ = error "Not supported: abs"
  signum _ = error "Not supported: signum"
  fromInteger x = Const x

-- An extended expression, one that we can use for computing the witness, but
-- which cannot be used in the circuit.
data ExprExt
  = Simple Expr
  | Div Expr Expr -- Integer division
  | Inv Expr      -- Inversion in the field
  | And Expr Expr -- Bitwise AND
  deriving (Eq, Ord, Show)
