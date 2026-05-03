module Pretty where

import Expr
import Data.List (intercalate)

pretty :: Expr -> String
pretty (Const x)
  | floor x == ceiling x = show $ floor x
  | otherwise = show x
pretty (Var str) = str ++ "_"
pretty (Sum [e]) = '+' : show e
pretty (Sum exprs) = "(" ++ intercalate " + " (map show exprs) ++ ")"
pretty (Mul [e]) = '*' : show e
pretty (Mul exprs) = intercalate " * " (map show exprs)
pretty (Pow base e) = "(" ++ show base ++ ")^" ++ show e
pretty (Fun f arg) = f ++ "(" ++ show arg ++ ")"
pretty Undefined = "Undefined"
