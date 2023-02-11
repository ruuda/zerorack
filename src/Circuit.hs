-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

module Circuit
  ( Constraint (..)
  , Definition (..)
  , Circuit (..)
  , newInput
  , buildCircuit
  , assertEq
  , fieldInv
  , idiv
  , bitAnd
  )
where

import Expr (Expr (Var), ExprExt (And, Div, Inv), VarNum (VarNum), Increment (inc))

data Constraint
  = ConstrainEq Expr Expr
  deriving (Eq, Ord, Show)

data Definition
  = Define VarNum ExprExt
  deriving (Eq, Ord, Show)

-- A monad for building circuits and constraints.
--
-- This is a hybrid between a state monad and a writer monad: the state is the
-- fresh variable counter, and the outputs to write are definitions (when we
-- need to define how to compute some intermediate variable, because it can't be
-- expressed in the circuit itself), and constraints.
data Circuit a = Circuit (VarNum -> (a, VarNum, [Definition], [Constraint]))

instance Functor Circuit where
  fmap :: (a -> b) -> Circuit a -> Circuit b
  fmap f (Circuit g) = Circuit $ \n ->
    let
      (x, n', ds, cs) = g n
    in
      (f x, n', ds, cs)

instance Applicative Circuit where
  pure :: a -> Circuit a
  pure x = Circuit $ \n -> (x, n, [], [])

  (<*>) :: Circuit (a -> b) -> Circuit a -> Circuit b
  (Circuit f) <*> (Circuit g) = Circuit $ \n ->
    let
      (vf, n',  dsf, csf) = f n
      (vx, n'', dsg, csg) = g n'
    in
      (vf vx, n'', dsg <> dsf, csg <> csf)

instance Monad Circuit where
  (>>=) :: Circuit a -> (a -> Circuit b) -> Circuit b
  (Circuit f) >>= g = Circuit $ \n ->
    let
      (x, n',  dsf, csf) = f n
      Circuit g' = g x
      (y, n'', dsg, csg) = g' n'
    in
      (y, n'', dsg <> dsf, csg <> csf)

-- Define a new input variable.
newInput :: Circuit Expr
newInput = Circuit $ \n -> (Var n, inc n, [], [])

buildCircuit :: Circuit a -> (a, [Definition], [Constraint])
buildCircuit (Circuit f) =
  let
    (x, _, ds, cs) = f $ VarNum 0
  in
    (x, ds, cs)

assertEq :: Expr -> Expr -> Circuit ()
assertEq x y = Circuit $ \n -> ((), n, [], [ConstrainEq x y])

-- Record the definition for a new variable as an extended expression.
define :: ExprExt -> Circuit Expr
define expr = Circuit $ \n -> (Var n, inc n, [Define n expr], [])

idiv :: Expr -> Expr -> Circuit Expr
idiv x y = define $ Div x y

fieldInv :: Expr -> Circuit Expr
fieldInv x = define $ Inv x

bitAnd :: Expr -> Expr -> Circuit Expr
bitAnd x y = define $ And x y
