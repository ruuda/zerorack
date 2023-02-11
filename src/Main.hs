-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (forM, forM_, when)
import Data.Foldable (foldr', for_)
import Data.IntMap.Strict (IntMap)
import Data.List (intercalate)

import qualified Data.IntMap.Strict as IntMap

import Expr (Expr (..), ExprExt (..), VarNum (..), RegNum (..), Increment (inc))
import Circuit (Constraint (..), Circuit, assertEq, bitAnd, buildCircuit, fieldInv, idiv, newInput)
import Compiler (Compile (..), Rank1Constraint (Rank1Constraint), Vector)

import qualified Compiler

-- If a vector only has a single nonzero coefficient, return the register
-- number and its coefficient.
isVecSingleton :: Vector -> Maybe (RegNum, Integer)
isVecSingleton x = case IntMap.toList x of
  [(k, z)] -> Just (RegNum k, z)
  _ -> Nothing

-- If a vector has only a coefficient for register 0 (the constant 1),
-- return the coefficient.
isVecConstant :: Vector -> Maybe Integer
isVecConstant x = case isVecSingleton x of
  Just (RegNum 0, z) -> Just z
  Nothing -> Nothing

-- Collect all contraints that are of the form "const * (linear combination) =
-- reg". These are "definitions", in the sense that we can replace all
-- references to register `reg` in other constraints by the left hand side of
-- the equality. Returns a map with every register of this form, and as value
-- the linear combination of other registers that it is equal to.
extractDefinitions :: [Rank1Constraint] -> IntMap Vector
extractDefinitions constraints =
  let

    -- If a constraint is a pure definition, add it to the map. A pure
    -- definition is one of the form "const * (linear combination) = reg".
    -- In this case we can replace all references to register `reg` in other
    -- constraints, with the left-hand side.
    addDefinition :: Rank1Constraint -> IntMap Vector -> IntMap Vector
    addDefinition constr defs = case constr of
      Rank1Constraint
        (isVecConstant -> Just a)
        b
        (isVecSingleton -> Just (RegNum r, 1))
        ->
          -- Fold the constant a into the coefficients of the vector b, then
          -- insert this as the definition of register r.
          IntMap.insert r (fmap (* a) b) defs
      _ ->
        defs
  in
    foldr' addDefinition IntMap.empty constraints

-- Try to express the same relations in fewer constraints.
optimizeConstraints :: [Rank1Constraint] -> [Rank1Constraint]
optimizeConstraints constraints =
  let
    defs = extractDefinitions constraints

    -- We drop all pure definitions from the list of constraints, assuming that
    -- we will execute the substitution elsewhere. TODO: Is this safe to do?
    -- In general we can't just drop constraints, are we sure that we don't
    -- change the solution space by dropping these?
    isRedundant :: Rank1Constraint -> Bool
    isRedundant = \case
      Rank1Constraint _ _ (isVecSingleton -> Just (RegNum r, 1)) -> IntMap.member r defs
      _ -> False

    -- Inspect a single term of the vector. The term consists of a coefficient
    -- and a register. Then, if the register has a definition, we add the
    -- expansion of that definition to the result. If not, then we just add the
    -- term itself to the result.
    addTerm :: Int -> Integer -> Vector -> Vector
    addTerm r coef v = case IntMap.lookup r defs of
      Just replacement -> IntMap.unionWith (+) v (fmap (* coef) replacement)
      Nothing          -> IntMap.insert r coef v

    substituteTerms :: Vector -> Vector
    substituteTerms = IntMap.foldrWithKey' addTerm IntMap.empty

    substitute :: Rank1Constraint -> Rank1Constraint
    substitute (Rank1Constraint a b c) = Rank1Constraint
      (substituteTerms a)
      (substituteTerms b)
      (substituteTerms c)
  in
    id
    $ fmap substitute
    $ filter (not . isRedundant)
    $ constraints

-- Return the k-th bit of the input expression.
selectBit :: Int -> Expr -> Circuit Expr
selectBit k x = do
  shifted <- x `idiv` (fromInteger $ 2 ^ k)
  bit <- shifted `bitAnd` fromInteger 1
  -- Constrain the auxiliary variable to be a single bit.
  assertEq (fromInteger 0) (bit * ((fromInteger 1) - bit))
  pure bit

assertBits4 :: Expr -> Circuit ()
assertBits4 x = do
  x0 <- selectBit 0 x
  x1 <- selectBit 1 x
  x2 <- selectBit 2 x
  x3 <- selectBit 3 x
  assertEq x (x0 + 2 * x1 + 4 * x2 + 8 * x3)

assertInRange1To9 :: Expr -> Circuit ()
assertInRange1To9 x = do
  assertBits4 $ x - 1
  assertBits4 $ x + 6

assertNonZero :: Expr -> Circuit ()
assertNonZero x = do
  xInv <- fieldInv x
  assertEq (fromInteger 1) (x * xInv)

assertAllDistinct :: [Expr] -> Circuit ()
assertAllDistinct xs = case xs of
  []  -> pure ()
  [_] -> pure ()
  x : xs -> do
    -- First of all, the tail must be distinct.
    assertAllDistinct xs
    -- Second, the head must be distinct from all elements in the tail.
    for_ xs $ \y -> assertNonZero $ x - y

assertRowGood :: [Expr] -> Circuit ()
assertRowGood xs = do
  assertAllDistinct xs
  for_ xs assertInRange1To9

main :: IO ()
main =
  let
    circuit = do
      inputs <- forM [1..9] $ \_ -> newInput
      assertRowGood inputs

    (_, ds, cs) = buildCircuit circuit
    (_, r1cs) = Compiler.runCompile $ forM cs Compiler.compileConstraint
    defs = extractDefinitions r1cs
    optR1cs = optimizeConstraints r1cs
    printDebug = False
  in do
    putStrLn "Definitions:"
    forM_ ds (putStrLn . show)
    putStrLn "\nConstraints:"
    forM_ cs (putStrLn . show)
    when printDebug $ do
      putStrLn "\nCompiled Rank-1 Constraints:"
      forM_ r1cs (putStrLn . show)
      putStrLn "\nPure definitions:"
      forM_ (IntMap.toList defs) (putStrLn . show)
    putStrLn "\nOptimized Rank-1 Constraints:"
    forM_ optR1cs (putStrLn . show)
