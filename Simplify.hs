module Simplify (automaticSimplify) where

import Data.List (partition, sort)
import Expr

simplifyPower :: Expr -> Int -> Expr
simplifyPower (Const a) b = Const (a ^ b)
simplifyPower Undefined _ = Undefined
simplifyPower base 0 = One
simplifyPower base 1 = base
simplifyPower (base :^ a) b = base :^ (a * b)
simplifyPower base exponent = base :^ exponent

simplifyProduct :: [Expr] -> Expr
simplifyProduct [] = One
simplifyProduct [a] = a
simplifyProduct exprs
  | Undefined `elem` exprs = Undefined
  | Zero `elem` exprs = Zero
  | otherwise = case simplifyProductRec exprs of
      [] -> One
      [a] -> a
      exprs' -> Mul exprs'

constHelper :: Double -> Expr -> [Expr]
constHelper a (Const b) | a == b = []
constHelper _ e = [e]

simplifyProductRec :: [Expr] -> [Expr]
simplifyProductRec [Const a, Const b] = constHelper 1 $ Const (a * b)
simplifyProductRec [One, b] = [b]
simplifyProductRec [a, One] = [a]
simplifyProductRec [Var x, Var y] | x == y = [Var x :^ 2]
simplifyProductRec [b :^ e, c :^ f] | b == c = constHelper 1 $ simplifyPower b (e + f)
simplifyProductRec [e, b :^ exp] | e == b = constHelper 1 $ simplifyPower b (exp + 1)
simplifyProductRec [b :^ exp, e] | e == b = constHelper 1 $ simplifyPower b (exp + 1)
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
simplifySum [] = Zero
simplifySum [a] = a
simplifySum exprs
  | Undefined `elem` exprs = Undefined
  | otherwise = case simplifySumRec exprs of
      [] -> Zero
      [a] -> a
      exprs' -> Sum exprs'

simplifySumRec :: [Expr] -> [Expr]
simplifySumRec [Const a, Const b] = constHelper 0 $ Const (a + b)
simplifySumRec [Zero, b] = [b]
simplifySumRec [a, Zero] = [a]
simplifySumRec [Var x, Var y] | x == y = [Const 2 :* Var x]
simplifySumRec [Const a :* e, Const b :* f] | e == f = constHelper 0 $ simplifyProduct [Const (a + b), e]
simplifySumRec [e, Const a :* f] | e == f = constHelper 0 $ simplifyProduct [Const (a + 1), e]
simplifySumRec [Const a :* e, f] | e == f = constHelper 0 $ simplifyProduct [Const (a + 1), e]
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
      | otherwise -> error "mergeSums error"

automaticSimplify :: Expr -> Expr
automaticSimplify (expr :^ exponent) = simplifyPower (automaticSimplify expr) exponent
automaticSimplify (Mul exprs) = simplifyProduct $ map automaticSimplify exprs
automaticSimplify (Sum exprs) = simplifySum $ map automaticSimplify exprs
automaticSimplify (f :@ arg) = f :@ automaticSimplify arg
automaticSimplify expr = expr
