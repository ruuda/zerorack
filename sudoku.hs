{-# LANGUAGE ApplicativeDo #-}

module Main where

-- Variables are numbered.
newtype VarNum = VarNum Int

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
-- which cannot be used in the expression.
data ExprExt
  = Simple Expr
  | Div Expr Expr -- Integer division
  | Inv Expr      -- Inversion in the field
  | And Expr Expr -- Bitwise AND
  deriving (Eq, Ord, Show)

data Output
  = ConstrainEq Expr Expr
  | Define VarNum ExtExpr
  deriving (Eq, Ord, Show)

type Circuit a = (a, [Output])

nextFreeVariable :: [Output] -> VarNum
nextFreeVariable = VarNum . maximum . getNum 
  where
    getNum (ConstrainEq _ _) = 0
    getNum (Define (VarNum i)) = i + 1

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

refreshOutput :: VarNum -> Output -> Output
refreshOutput n out = case out of
  ConstrainEq x y -> ConstrainEq (refresh n x) (refresh n y)
  Define i expr -> Define (bump n i) (refresh n expr)

instance Functor Circuit where
  fmap :: (a -> b) -> Circuit a -> Circuit b
  fmap f (x, cs) = (f x, cs)

instance Applicative Circuit where
  pure :: a -> Circuit a
  pure x = (x, [])

  -- We concatenate the outputs of each of the sides, but we also have to
  -- renumber all variables that occur on the right-hand side to not clash with
  -- the ones defined on the left-hand side
  (<*>) :: Circuit (a -> b) -> Circuit a -> Circuit b
  (f, csa) <*> (x, csb) = (f x, csa <> (fmap (refreshOutput $ nextFreeVariable csa) csb)

assertEq :: Expr -> Expr -> Circuit ()
assertEq x y = ((), [ConstrainEq x y])

div :: Expr -> Expr -> Circuit Expr
div x y = (Var (VarNum 0), [Define (VarNum 0) (Div x y)])

inv :: Expr -> Circuit Expr
inv x = (Var (VarNum 0), [Define (VarNum 0) (Inv x)])

bits4 :: Expr -> Circuit ()
bits4 x = do
  x0 <- div
