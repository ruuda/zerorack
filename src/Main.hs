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

data Constraint
  = ConstrainEq Expr Expr
  deriving (Eq, Ord, Show)

data Definition
  = Define VarNum ExprExt
  deriving (Eq, Ord, Show)

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
    (_, r1cs) = runCompile $ forM cs compileConstraint
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
