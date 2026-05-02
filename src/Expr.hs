{-# LANGUAGE PatternSynonyms #-}

module Expr where

import Data.List (intercalate)

-- TODO: allow integers/fractions, implement negation?
data Expr
  = Const Double
  | Var String
  | Sum [Expr]
  | Mul [Expr]
  | Pow Expr Int
  | Fun String Expr
  | Undefined
  deriving (Eq)

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
  compare (Fun _ _) (Var _) = GT
  compare Undefined _ = GT
  compare a b = compare EQ (compare b a)

instance Show Expr where
  show (Const x)
    | floor x == ceiling x = show $ floor x
    | otherwise = show x
  show (Var str) = str ++ "_"
  show (Sum [e]) = '+' : show e
  show (Sum exprs) = "(" ++ intercalate " + " (map show exprs) ++ ")"
  show (Mul [e]) = '*' : show e
  show (Mul exprs) = intercalate " * " (map show exprs)
  show (Pow base e) = "(" ++ show base ++ ")^" ++ show e
  show (Fun f arg) = f ++ "(" ++ show arg ++ ")"
  show Undefined = "Undefined"

pattern Zero :: Expr
pattern Zero = Const 0

pattern One :: Expr
pattern One = Const 1

pattern (:+) :: Expr -> Expr -> Expr
pattern a :+ b = Sum [a, b]

pattern (:*) :: Expr -> Expr -> Expr
pattern a :* b = Mul [a, b]

pattern (:^) :: Expr -> Int -> Expr
pattern b :^ e = Pow b e

pattern (:@) :: String -> Expr -> Expr
pattern f :@ arg = Fun f arg
