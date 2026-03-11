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
  deriving (Eq, Ord)

instance Show Expr where
  show (Const x) = show x
  show (Var str) = str
  show (Sum exprs) = "(" ++ intercalate " + " (map show exprs) ++ ")"
  show (Mul exprs) = intercalate " * " (map show exprs)
  show (Pow expr exp) = "(" ++ show expr ++ ")^" ++ show exp

reduceExpr :: Expr -> Expr
reduceExpr (Sum []) = Const 0
reduceExpr (Mul []) = Const 1
reduceExpr (Sum [a]) = a
reduceExpr (Mul [a]) = a
reduceExpr (Pow (Const a) b) = Const (a ^ b)
reduceExpr (Pow expr 1) = expr
reduceExpr (Pow expr 0) = Const 1
reduceExpr expr = expr

isConst :: Expr -> Bool
isConst (Const _) = True
isConst _ = False

groupConsts' :: ([Expr] -> Expr) -> ([Double] -> Double) -> [Expr] -> Expr
groupConsts' ctor f exprs =
  case partition isConst exprs of
    ([], _) -> ctor exprs
    (cs, rest) -> ctor (Const (f [n | Const n <- cs]) : rest)

groupConsts :: Expr -> Expr
groupConsts (Sum exprs) = groupConsts' Sum sum exprs
groupConsts (Mul exprs) = groupConsts' Mul product exprs
groupConsts expr = expr

isPower :: Expr -> Bool
isPower (Pow _ _) = True
isPower _ = False

combinePowers :: [Expr] -> [Expr]
combinePowers [] = []
combinePowers [e] = [e]
combinePowers (Pow b e : Pow c f : es)
  | b == c = combinePowers (Pow b (e + f) : es)
  | otherwise = Pow b e : combinePowers (Pow c f : es)

groupPowers :: Expr -> Expr
groupPowers (Mul exprs) = Mul . combinePowers . sort $ powers ++ exprs'
  where
    powers = filter isPower exprs
    exprs' = filter (not . isPower) exprs

-- simplify' :: Expr -> Expr
-- simplify' (Mul exprs)
--   | Const 0 `elem` exprs = Const 0
--   | otherwise = groupConsts (Mul exprs)
-- simplify' expr = expr
--
-- simplify :: Expr -> Expr
-- simplify (Sum exprs) = simplify' . Sum $ map simplify exprs
-- simplify (Mul exprs) = simplify' . Mul $ map simplify exprs
-- simplify (Pow expr power) = simplify' (Pow (simplify expr) power)
-- simplify expr = expr
