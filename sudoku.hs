{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (forM_)
import Data.Foldable (for_)

-- Variables are numbered.
newtype VarNum = VarNum Int deriving (Eq, Ord)

instance Show VarNum where
  show (VarNum i) = "x" <> (show i)

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

data Output
  = ConstrainEq Expr Expr
  | Define VarNum ExprExt
  deriving (Eq, Ord, Show)

data Circuit a = Circuit a [Output] deriving (Eq, Ord, Show)

nextFreeVariable :: [Output] -> VarNum
nextFreeVariable [] = VarNum 0
nextFreeVariable xs = VarNum $ maximum $ fmap getNum xs
  where
    getNum :: Output -> Int
    getNum (ConstrainEq _ _) = 0
    getNum (Define (VarNum i) _) = i + 1

bump :: VarNum -> VarNum -> VarNum
bump (VarNum x) (VarNum y) = VarNum (x + y)

refresh :: VarNum -> Expr -> Expr
refresh n expr = case expr of
  Const k -> Const k
  Add x y -> Add (refresh n x) (refresh n y)
  Sub x y -> Sub (refresh n x) (refresh n y)
  Mul x y -> Mul (refresh n x) (refresh n y)
  Var i -> Var $ bump n i

refreshExt :: VarNum -> ExprExt -> ExprExt
refreshExt n expr = case expr of
  Simple ex -> Simple (refresh n ex)
  Div x y   -> Div (refresh n x) (refresh n y)
  Inv x     -> Inv (refresh n x)
  And x y   -> And (refresh n x) (refresh n y)

refreshOutput :: VarNum -> Output -> Output
refreshOutput n out = case out of
  ConstrainEq x y -> ConstrainEq (refresh n x) (refresh n y)
  Define i expr   -> Define (bump n i) (refreshExt n expr)

instance Functor Circuit where
  fmap :: (a -> b) -> Circuit a -> Circuit b
  fmap f (Circuit x cs) = Circuit (f x) cs

instance Applicative Circuit where
  pure :: a -> Circuit a
  pure x = Circuit x []

  -- We concatenate the outputs of each of the sides, but we also have to
  -- renumber all variables that occur on the right-hand side to not clash with
  -- the ones defined on the left-hand side
  (<*>) :: Circuit (a -> b) -> Circuit a -> Circuit b
  (Circuit f csa) <*> (Circuit x csb) =
    Circuit (f x) (csa <> (fmap (refreshOutput $ nextFreeVariable csa) csb))

-- TODO: Monad instance considered harmful, can we avoid it?
instance Monad Circuit where
  (>>=) :: Circuit a -> (a -> Circuit b) -> Circuit b
  -- TODO: Renumber, don't lose the csa
  (Circuit x csa) >>= f =
    let
      Circuit y csb = f x
    in
      Circuit y (csa <> (fmap (refreshOutput $ nextFreeVariable csa) csb))

assertEq :: Expr -> Expr -> Circuit ()
assertEq x y = Circuit () [ConstrainEq x y]

-- Record the definition for a new variable as an extended expression.
define :: ExprExt -> Circuit Expr
define expr = Circuit (Var (VarNum 0)) [Define (VarNum 0) expr]

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
    -- Hack: number our own inputs negatively, because right now I don't scan in
    -- expressions to find the next free variable.
    inputs = fmap (Var . VarNum . negate) [1..9]
    Circuit _ outputs = assertRowGood inputs
  in
    forM_ outputs (putStrLn . show)
