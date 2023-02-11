-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM, forM_, when)
import Data.Foldable (for_)

import qualified Data.IntMap.Strict as IntMap

import Expr (Expr)
import Circuit (Circuit, assertEq, bitAnd, buildCircuit, fieldInv, idiv, newInput)

import qualified Compiler
import qualified Optimizer

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
assertAllDistinct = \case
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
      inputs <- forM [1..9 :: Int] $ \_ -> newInput
      assertRowGood inputs

    (_, ds, cs) = buildCircuit circuit
    (_, r1cs) = Compiler.runCompile $ forM cs Compiler.compileConstraint
    defs = Optimizer.extractDefinitions r1cs
    optR1cs = Optimizer.optimizeConstraints r1cs
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
