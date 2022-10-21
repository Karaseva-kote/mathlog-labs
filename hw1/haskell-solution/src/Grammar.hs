module Grammar where

import Data.List (intercalate)

data Binop = Impl 
           | Or 
           | And
           deriving (Eq, Ord)

instance Show Binop where
  show Impl = "->"
  show Or   = "|"
  show And  = "&"

data Expr = Binary { getOp :: Binop, getLeft :: Expr, getRight :: Expr}
          | Not Expr
          | Var String
          | Lie
          deriving (Eq, Ord)

instance Show Expr where
  show (Binary op a b) = "(" ++ show a ++ show op ++ show b ++ ")"
  show (Not e)         = "(" ++ show e ++ "->_|_)"
  show (Var name)      = name
  show Lie             = "_|_"

data Problem = Problem { getContext :: [Expr], getExpr :: Expr }

instance Show Problem where
  show (Problem list expr) = show list ++ " |- " ++ show expr

data Proof = Axiom { getNum :: Int, getA :: Expr, getB :: Expr, getC :: Expr, getExp :: Expr }
           | Hypothesis { getNum :: Int, getExp :: Expr }
           | ModusPonens { getFirst :: Proof, getSecond :: Proof, getExp :: Expr }
           | MyNothing { getNum :: Int }
           deriving (Eq, Ord)

instance Show Proof where
  show (Axiom num a b c e) = "axiom " ++ show num
  show (Hypothesis num exp) = "hypothesis " ++ show num
  show (ModusPonens l r exp) = "ModusPonens " ++ show l ++ " " ++ show r
  show (MyNothing n) = "Nothing "