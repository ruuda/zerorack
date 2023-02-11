-- Zerorack -- Experiments with circuit compilation and zk-snarks
-- Copyright 2023 Ruud van Asseldonk

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Optimizer
  ( extractDefinitions
  , optimizeConstraints
  )
where

import Data.Foldable (foldr')
import Data.IntMap.Strict (IntMap)

import qualified Data.IntMap.Strict as IntMap

import Expr (RegNum (RegNum))
import Compiler (Rank1Constraint (Rank1Constraint), Vector)

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

