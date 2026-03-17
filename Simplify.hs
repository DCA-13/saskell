module Simplify (automaticSimplify) where

import Data.List (partition, sort)
import Expr

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

constHelper :: Double -> Expr -> [Expr]
constHelper a (Const b) | a == b = []
constHelper _ e = [e]

simplifyProductRec :: [Expr] -> [Expr]
simplifyProductRec [Const a, Const b] = constHelper 1 $ Const (a * b)
simplifyProductRec [Const 1, b] = [b]
simplifyProductRec [a, Const 1] = [a]
simplifyProductRec [Var x, Var y] | x == y = [Pow (Var x) 2]
simplifyProductRec [Pow b e, Pow c f] | b == c = constHelper 1 $ simplifyPower b (e + f)
simplifyProductRec [e, Pow b exp] | e == b = constHelper 1 $ simplifyPower b (exp + 1)
simplifyProductRec [Pow b exp, e] | e == b = constHelper 1 $ simplifyPower b (exp + 1)
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
simplifySumRec [Const a, Const b] = constHelper 0 $ Const (a + b)
simplifySumRec [Const 0, b] = [b]
simplifySumRec [a, Const 0] = [a]
simplifySumRec [Var x, Var y] | x == y = [Mul [Const 2, Var x]]
simplifySumRec [Mul [Const a, e], Mul [Const b, f]] | e == f = constHelper 0 $ simplifyProduct [Const (a + b), e]
simplifySumRec [e, Mul [Const a, f]] | e == f = constHelper 0 $ simplifyProduct [Const (a + 1), e]
simplifySumRec [Mul [Const a, e], f] | e == f = constHelper 0 $ simplifyProduct [Const (a + 1), e]
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
    -- [_, _] -> mergeSums' (acc ++ [a]) pss qss
    --   where
    --     [a, b] = sort [p, q]
    --     pss = if a == p then ps else p : ps
    --     qss = if a == q then qs else q : qs
    [a, b]
      | a == p && b == q -> mergeSums' (acc ++ [p]) ps (q : qs)
      | a == q && b == p -> mergeSums' (acc ++ [q]) (p : ps) qs
      | otherwise -> error "mergeSums error"

automaticSimplify :: Expr -> Expr
automaticSimplify (Pow expr exponent) = simplifyPower (automaticSimplify expr) exponent
automaticSimplify (Mul exprs) = simplifyProduct $ map automaticSimplify exprs
automaticSimplify (Sum exprs) = simplifySum $ map automaticSimplify exprs
automaticSimplify (Fun f arg) = Fun f $ automaticSimplify arg
automaticSimplify expr = expr
