module Pretty where

import Expr
import Data.List (intercalate)

pretty :: Expr -> String
pretty (Const x)
  | (floor x :: Int) == (ceiling x :: Int) = show (floor x :: Int)
  | otherwise = show x
pretty (Var str) = str ++ "_"
pretty (Sum [e]) = '+' : pretty e
pretty (Sum exprs) = "(" ++ intercalate " + " (map pretty exprs) ++ ")"
pretty (Mul [e]) = '*' : pretty e
pretty (Mul exprs) = intercalate " * " (map pretty exprs)
pretty (Pow base e) = "(" ++ pretty base ++ ")^" ++ show e
pretty (Fun f arg) = f ++ "[" ++ pretty arg ++ "]"
pretty Undefined = "Undefined"
