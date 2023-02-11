-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Compiler
  ( Vector
  , Rank1Constraint (..)
  , Compile (..)
  , compileConstraint
  , runCompile
  )
where

import Data.IntMap.Strict (IntMap)
import Data.List (intercalate)

import qualified Data.IntMap.Strict as IntMap

import Expr (Expr (Add, Const, Mul, Sub, Var), RegNum (RegNum), VarNum (VarNum), inc)
import Circuit (Constraint (ConstrainEq))

-- A vector of field elements, represented sparsely
-- as a map of index to value.
type Vector = IntMap Integer

-- A constraint of the form <w, a> * <w, b> = <w, c>
-- where <x, y> is the inner product between two vectors,
-- and * is the regular product in the field.
data Rank1Constraint = Rank1Constraint Vector Vector Vector deriving (Eq, Ord)

instance Show Rank1Constraint where
  show (Rank1Constraint a b c) =
    let
      -- Register 0 contains value 1, so we omit it. Similarly, if the
      -- coefficient is 1, we can omit it.
      showProduct (0, v) = show v
      showProduct (k, 1) = "r" <> (show k)
      showProduct (k, v) = (show v) <> " r" <> (show k)

      showInnerProduct = id
        . intercalate " + "
        . fmap showProduct
        . IntMap.toList

      showParens xs = case IntMap.size xs of
        0 -> "0"
        1 -> showInnerProduct xs
        _ -> "(" <> (showInnerProduct xs) <> ")"
    in
      (showParens a) <> " * " <> (showParens b) <> " = " <> (showInnerProduct c)

-- The compiler state tracks the next free register, as well as which registers
-- should be filled by which variables. (Registers that don't occur in the
-- variable mapping are intermediate registers that were introduced to make
-- constraints rank 1.)
data CompileState = CompileState
  { csNextFree :: RegNum
  , csVariableMapping :: IntMap RegNum
  }

data Compile a = Compile (CompileState -> (a, CompileState, [Rank1Constraint]))

instance Functor Compile where
  fmap :: (a -> b) -> Compile a -> Compile b
  fmap f (Compile g) = Compile $ \n ->
    let
      (x, n', cs) = g n
    in
      (f x, n', cs)

instance Applicative Compile where
  pure :: a -> Compile a
  pure x = Compile $ \n -> (x, n, [])

  (<*>) :: Compile (a -> b) -> Compile a -> Compile b
  (Compile f) <*> (Compile g) = Compile $ \n ->
    let
      (vf, n',  csf) = f n
      (vx, n'', csg) = g n'
    in
      (vf vx, n'', csg <> csf)

instance Monad Compile where
  (>>=) :: Compile a -> (a -> Compile b) -> Compile b
  (Compile f) >>= g = Compile $ \n ->
    let
      (x, n',  csf) = f n
      Compile g' = g x
      (y, n'', csg) = g' n'
    in
      (y, n'', csg <> csf)

-- Emit a constraint of the form <w,a> * <w,b> = <w,c>
emitConstraint :: Vector -> Vector -> Vector -> Compile ()
emitConstraint a b c = Compile $ \n -> ((), n, [Rank1Constraint a b c])

newRegister :: Compile RegNum
newRegister = Compile $
  \(CompileState n regs) -> (n, CompileState (inc n) regs, [])

-- Return the register that is mapped to the given variable, or insert this
-- mapping if one does not yet exist.
mapVariable :: VarNum -> Compile RegNum
mapVariable (VarNum i) = Compile $
  \(CompileState n regs) -> case IntMap.lookup i regs of
    Just r  -> (r, CompileState n regs, [])
    Nothing -> (n, CompileState (inc n) (IntMap.insert i n regs), [])

runCompile :: Compile a -> (a, [Rank1Constraint])
runCompile (Compile f) =
  let
    -- Register 0 is always implicitly 1,
    -- the next free register is number 1.
    (x, _, cs) = f $ CompileState (RegNum 1) IntMap.empty
  in
    (x, cs)

compileExpr :: Expr -> Compile Vector
compileExpr = \case
  Const x ->
    -- A constant x we can represent as the inner product x * r0,
    -- where r0 holds the constant 1.
    pure $ IntMap.singleton 0 x

  Add x y -> do
    -- If we add two expressions, and we have both of them as a list of
    -- coefficients, then we can just add those coefficients elementwise.
    xv <- compileExpr x
    yv <- compileExpr y
    pure
      $ IntMap.filter (/= 0)
      $ IntMap.unionWith (+) xv yv

  Sub x y -> do
    -- Same for subtraction.
    xv <- compileExpr x
    yv <- compileExpr y
    pure
      $ IntMap.filter (/= 0)
      $ IntMap.unionWith (-) xv yv

  Mul x y -> do
    -- To multiply two expressions, we introduce a new R1CS constraint with a
    -- fresh register. The register holds the result of the multiplication, and
    -- then we constrain it to be equal to the product of the inputs.
    xv <- compileExpr x
    yv <- compileExpr y
    RegNum r <- newRegister
    emitConstraint xv yv (IntMap.singleton r 1)
    pure $ IntMap.singleton r 1

  Var v -> do
    RegNum r <- mapVariable v
    pure $ IntMap.singleton r 1

-- Put the expression in a canonical form. This helps to simplify the pattern
-- matching later; we don't have to consider all the symmetries of a particular
-- situation.
canonicalizeExpr :: Expr -> Expr
canonicalizeExpr = \case
  -- Float constants to the left in commutative operators.
  Add x (Const z) -> Add (Const z) (canonicalizeExpr x)
  Mul x (Const z) -> Mul (Const z) (canonicalizeExpr x)
  -- Float Variables to the right
  Add (Var i) x -> Add (canonicalizeExpr x) (Var i)
  Mul (Var i) x -> Mul (canonicalizeExpr x) (Var i)

  -- Otherwise, canonicalize internally.
  Add x y -> Add (canonicalizeExpr x) (canonicalizeExpr y)
  Sub x y -> Sub (canonicalizeExpr x) (canonicalizeExpr y)
  Mul x y -> Mul (canonicalizeExpr x) (canonicalizeExpr y)
  Const z -> Const z
  Var i -> Var i

canonicalizeConstraint :: Constraint -> Constraint
canonicalizeConstraint (ConstrainEq x y) = case (canonicalizeExpr x, canonicalizeExpr y) of
  -- Float multiplications to the left, and additions to the right.
  (x, Mul y z) -> ConstrainEq (Mul y z) x
  (Add x y, z) -> ConstrainEq z (Add x y)
  (Sub x y, z) -> ConstrainEq z (Sub x y)
  -- Float variables to the left, and constants to the right.
  (x, Var i)   -> ConstrainEq (Var i) x
  (Const z, x) -> ConstrainEq x (Const z)
  -- Otherwise leave the constraints untouched.
  (x, y)       -> ConstrainEq x y

compileConstraint :: Constraint -> Compile ()
compileConstraint constr = case canonicalizeConstraint constr of
  -- The final general case can deal with any kind of expression, but we
  -- special-case constraints of the form "product = constant", because that
  -- form matches the R1CS form, so we can compile it to that with fewer
  -- intermediate registers.
  ConstrainEq (Mul xa xb) y -> do
    ra <- compileExpr xa
    rb <- compileExpr xb
    ry <- compileExpr y
    emitConstraint ra rb ry

  ConstrainEq x y -> do
    rx <- compileExpr x
    ry <- compileExpr y
    -- TODO: If it's a multiplication, we can use it directly.
    -- Or maybe we should just run an optimizer afterwards to pack everything
    -- tightly again. I have to think about this ...
    -- Emit a constraint of the form 1 * x = y.
    emitConstraint rx (IntMap.singleton 0 1) ry

