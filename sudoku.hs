{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Foldable (for_)

-- Variables are numbered.
newtype VarNum = VarNum Int deriving (Eq, Ord)

instance Show VarNum where
  show (VarNum i) = "x" <> (show i)

inc :: VarNum -> VarNum
inc (VarNum i) = VarNum (i + 1)

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
  abs x = error "Not supported: abs"
  signum x = error "Not supported: signum"
  fromInteger x = Const x

-- An extended expression, one that we can use for computing the witness, but
-- which cannot be used in the circuit.
data ExprExt
  = Simple Expr
  | Div Expr Expr -- Integer division
  | Inv Expr      -- Inversion in the field
  | And Expr Expr -- Bitwise AND
  deriving (Eq, Ord, Show)

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
  in do
    forM_ ds (putStrLn . show)
    forM_ cs (putStrLn . show)
