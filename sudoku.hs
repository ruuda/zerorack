{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Foldable (for_)
import Data.IntMap (IntMap)
import Data.List (intercalate)

import qualified Data.IntMap as IntMap

-- Variables are numbered.
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

data Compile a = Compile (RegNum -> (a, RegNum, [Rank1Constraint]))

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
newRegister = Compile $ \n -> (n, inc n, [])

runCompile :: Compile a -> (a, [Rank1Constraint])
runCompile (Compile f) =
  let
    -- Register 0 is always implicitly 1,
    -- the next free register is number 1.
    (x, _, cs) = f (RegNum 1)
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
    -- TODO: Record somewhere that register r should be filled with variable v.
    RegNum r <- newRegister
    pure $ IntMap.singleton r 1

compileConstraint :: Constraint -> Compile ()
compileConstraint (ConstrainEq x y) = case (x, y) of
  -- The final general case can deal with any kind of expression, but we
  -- special-case constraints of the form "product = constant", because that
  -- form matches the R1CS form, so we can compile it to that with fewer
  -- intermediate registers.
  (Mul xa xb, _y) -> do
    ra <- compileExpr xa
    rb <- compileExpr xb
    ry <- compileExpr y
    emitConstraint ra rb ry

  (_x, Mul ya yb) -> do
    rx <- compileExpr x
    ra <- compileExpr ya
    rb <- compileExpr yb
    emitConstraint ra rb rx

  _ -> do
    rx <- compileExpr x
    ry <- compileExpr y
    -- TODO: If it's a multiplication, we can use it directly.
    -- Or maybe we should just run an optimizer afterwards to pack everything
    -- tightly again. I have to think about this ...
    -- Emit a constraint of the form 1 * x = y.
    emitConstraint rx (IntMap.singleton 0 1) ry

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
  in do
    putStrLn "Definitions:"
    forM_ ds (putStrLn . show)
    putStrLn "\nConstraints:"
    forM_ cs (putStrLn . show)
    putStrLn "\nCompiled Rank-1 Constraints:"
    forM_ r1cs (putStrLn . show)
