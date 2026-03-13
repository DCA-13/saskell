module Main where

import Data.List (intercalate, partition, sort)

main :: IO ()
main = do
  putStrLn "Hello World!"

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

simplifyPower :: Expr -> Int -> Expr
simplifyPower (Const a) b = Const (a ^ b)
simplifyPower Undefined _ = Undefined
simplifyPower base 0 = Const 1
simplifyPower base 1 = base
simplifyPower (Pow base a) b = Pow base (a * b)
simplifyPower base exponent = Pow base exponent

simplifyProduct :: [Expr] -> Expr
simplifyProduct [] = Const 1
simplifyProduct [a] = a
simplifyProduct exprs
  | Undefined `elem` exprs = Undefined
  | Const 0 `elem` exprs = Const 0
  | otherwise = case simplifyProductRec exprs of
      [] -> Const 1
      [a] -> a
      exprs' -> Mul exprs'

simplifyProductRec :: [Expr] -> [Expr]
simplifyProductRec [Const a, Const b] =
  case a * b of
    1 -> []
    c -> [Const c]
simplifyProductRec [Const 1, b] = [b]
simplifyProductRec [a, Const 1] = [a]
simplifyProductRec [Var x, Var y]
  | x == y = [Pow (Var x) 2]
  | otherwise = sort [Var x, Var y]
simplifyProductRec [Var x, Pow c f]
  | Var x == c = [Pow c (1 + f)]
  | otherwise = sort [Var x, Pow c f]
simplifyProductRec [Pow b e, Var x]
  | b == Var x = [Pow b (1 + e)]
  | otherwise = sort [Pow b e, Var x]
simplifyProductRec [Pow b e, Pow c f]
  | b == c = case simplifyPower b (e + f) of
      Const 1 -> []
      p -> [p]
  | otherwise = sort [Pow b e, Pow c f]
simplifyProductRec [Mul e1, Mul e2] = mergeProducts e1 e2
simplifyProductRec [Mul e1, e2] = mergeProducts e1 [e2]
simplifyProductRec [e1, Mul e2] = mergeProducts [e1] e2
simplifyProductRec [e1, e2] = sort [e1, e2]
simplifyProductRec (e : es) = mergeProducts prods (simplifyProductRec es)
  where
    prods = case e of
      Mul exprs -> exprs
      expr -> [expr]

mergeProducts :: [Expr] -> [Expr] -> [Expr]
mergeProducts = mergeProducts' []

mergeProducts' :: [Expr] -> [Expr] -> [Expr] -> [Expr]
mergeProducts' acc [] qs = acc ++ qs
mergeProducts' acc ps [] = acc ++ ps
mergeProducts' acc (p : ps) (q : qs) =
  case simplifyProductRec [p, q] of
    [] -> mergeProducts' acc ps qs
    [h] -> mergeProducts' (acc ++ [h]) ps qs
    [a, b]
      | a == p && b == q -> mergeProducts' (acc ++ [p]) ps (q : qs)
      | a == q && b == p -> mergeProducts' (acc ++ [q]) (p : ps) qs
      | otherwise -> [Undefined]

simplifySum :: [Expr] -> Expr
simplifySum [] = Const 0
simplifySum [a] = a
simplifySum exprs
  | Undefined `elem` exprs = Undefined
  | otherwise = case simplifySumRec exprs of
      [] -> Const 0
      [a] -> a
      exprs' -> Sum exprs'

simplifySumRec :: [Expr] -> [Expr]
simplifySumRec [Const a, Const b] =
  case a + b of
    0 -> []
    c -> [Const c]
simplifySumRec [Const 0, b] = [b]
simplifySumRec [a, Const 0] = [a]
simplifySumRec [Var x, Var y] = simplifySumRec [Mul [Const 1, Var x], Mul [Const 1, Var y]]
simplifySumRec [Var x, e] = simplifySumRec [Mul [Const 1, Var x], e]
simplifySumRec [e, Var x] = simplifySumRec [e, Mul [Const 1, Var x]]
simplifySumRec [Mul [Const a, Var x], Mul [Const b, Var y]]
  | x == y = case simplifyProduct [Const (a + b), Var x] of
      Const 0 -> []
      p -> [p]
  | otherwise = sort [Mul [Const a, Var x], Mul [Const b, Var y]]
simplifySumRec [Sum e1, Sum e2] = mergeSums e1 e2
simplifySumRec [Sum e1, e2] = mergeSums e1 [e2]
simplifySumRec [e1, Sum e2] = mergeSums [e1] e2
simplifySumRec [e1, e2] = sort [e1, e2]
simplifySumRec (e : es) = mergeSums sums (simplifySumRec es)
  where
    sums = case e of
      Sum exprs -> exprs
      expr -> [expr]

mergeSums :: [Expr] -> [Expr] -> [Expr]
mergeSums = mergeSums' []

mergeSums' :: [Expr] -> [Expr] -> [Expr] -> [Expr]
mergeSums' acc [] qs = acc ++ qs
mergeSums' acc ps [] = acc ++ ps
mergeSums' acc (p : ps) (q : qs) =
  case simplifySumRec [p, q] of
    [] -> mergeSums' acc ps qs
    [h] -> mergeSums' (acc ++ [h]) ps qs
    [a, b]
      | a == p && b == q -> mergeSums' (acc ++ [p]) ps (q : qs)
      | a == q && b == p -> mergeSums' (acc ++ [q]) (p : ps) qs
      | otherwise -> [Undefined]

automaticSimplify :: Expr -> Expr
automaticSimplify (Pow expr exponent) = simplifyPower (automaticSimplify expr) exponent
automaticSimplify (Mul exprs) = simplifyProduct $ map automaticSimplify exprs
automaticSimplify (Sum exprs) = simplifySum $ map automaticSimplify exprs
automaticSimplify expr = expr
