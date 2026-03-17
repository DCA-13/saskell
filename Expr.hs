module Expr where

import Data.List (intercalate)

data Expr
  = Const Double
  | Var String
  | Sum [Expr]
  | Mul [Expr]
  | Pow Expr Int
  | Fun String Expr
  | Undefined

instance Eq Expr where
  Const a == Const b = a == b
  Var x == Var y = x == y
  Sum e == Sum f = e == f
  Mul e == Mul f = e == f
  Pow b e == Pow c f = b == c && e == f
  Fun f a == Fun g b = f == g && a == b
  Undefined == Undefined = True
  _ == _ = False

instance Ord Expr where
  compare (Const a) (Const b) = compare a b
  compare (Var x) (Var y) = compare x y
  compare (Sum es) (Sum fs) = compare (reverse es) (reverse fs)
  compare (Mul es) (Mul fs) = compare (reverse es) (reverse fs)
  compare (Pow b e) (Pow c f) = compare b c <> compare e f
  compare (Fun f a) (Fun g b) = compare f g <> compare a b
  compare (Const _) _ = LT
  compare _ (Const _) = GT
  compare (Mul es) e = compare (Mul es) (Mul [e])
  compare (Pow b e) (Var x) = compare (Pow b e) (Pow (Var x) 1)
  compare (Pow b e) (Sum es) = compare (Pow b e) (Pow (Sum es) 1)
  compare (Pow b e) (Fun f a) = compare (Pow b e) (Pow (Fun f a) 1)
  compare (Sum es) (Var x) = compare (Sum es) (Sum [Var x])
  compare (Sum es) (Fun f a) = compare (Sum es) (Sum [Fun f a])
  compare (Fun f a) (Var x) = GT
  compare Undefined _ = GT
  compare a b = compare EQ (compare b a)

instance Show Expr where
  show (Const x)
    | floor x == ceiling x = show $ floor x
    | otherwise = show x
  show (Var str) = str
  show (Sum exprs) = "(" ++ intercalate " + " (map show exprs) ++ ")"
  show (Mul exprs) = intercalate " * " (map show exprs)
  show (Pow base exponent) = "(" ++ show base ++ ")^" ++ show exponent
  show (Fun f arg) = f ++ "(" ++ show arg ++ ")"
  show Undefined = "Undefined"
