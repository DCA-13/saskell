module Expr where

import Data.List (intercalate)

data Expr
  = Const Double
  | Var String
  | Sum [Expr]
  | Mul [Expr]
  | Pow Expr Int
  | Undefined

instance Ord Expr where
  Const a <= Const b = a <= b
  Var x <= Var y = x <= y
  Sum [] <= Sum _ = True
  Sum _ <= Sum [] = False
  Sum es <= Sum fs
    | last es /= last fs = last es < last fs
    | otherwise = Sum (init es) <= Sum (init fs)
  Mul es <= Mul fs = Sum es <= Sum fs
  Pow b e <= Pow c f
    | b /= c = b <= c
    | otherwise = e <= f
  _ <= Const _ = False
  Const a <= Var x = True
  Const a <= Mul es = Mul [Const a] <= Mul es
  Const a <= Sum es = Sum [Const a] <= Sum es
  Const a <= Pow b e = Pow (Const a) 1 <= Pow b e
  Mul es <= e = Mul es <= Mul [e]
  Pow b e <= Var x = Pow b e <= Pow (Var x) 1
  Pow b e <= Sum es = Pow b e <= Pow (Sum es) 1
  Sum es <= Var x = Sum es <= Sum [Var x]
  e <= f = e == f || e < f

instance Show Expr where
  show (Const x)
    | floor x == ceiling x = show $ floor x
    | otherwise = show x
  show (Var str) = str
  show (Sum exprs) = "(" ++ intercalate " + " (map show exprs) ++ ")"
  show (Mul exprs) = intercalate " * " (map show exprs)
  show (Pow base exponent) = "(" ++ show base ++ ")^" ++ show exponent
  show Undefined = "Undefined"

instance Eq Expr where
  Const a == Const b = a == b
  Var x == Var y = x == y
  Sum e == Sum f = e == f
  Mul e == Mul f = e == f
  Pow b e == Pow c f = b == c && e == f
  Undefined == Undefined = True
  Var x == Pow (Var y) 1 = x == y
  Pow (Var x) 1 == Var y = x == y
  (Var x) == (Mul [Const 1, Var y]) = x == y
  Mul [Const 1, Var x] == Var y = x == y
  _ == _ = False
