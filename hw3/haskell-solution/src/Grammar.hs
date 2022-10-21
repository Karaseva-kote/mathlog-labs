module Grammar where

import Data.List (intercalate)

data Binop = Impl
           | Or
           | And
           deriving (Eq, Ord)

instance Show Binop where
  show Impl  = "->"
  show Or    = "|"
  show And   = "&"

data Quantifier = Forall
                | Exist
                deriving (Eq, Ord)

instance Show Quantifier where
  show Forall = "@"
  show Exist  = "?"

data Expr = Binary { getOp :: Binop, getLeft :: Expr, getRight :: Expr}
          | Quantifiers { getQuant :: Quantifier, getVar :: String, getExpr :: Expr}
          | Not Expr
          | PVar String
          | Equal Term Term
          deriving (Eq, Ord)

instance Show Expr where
  show (Binary op a b)     = "(" ++ show a ++ show op ++ show b ++ ")"
  show (Quantifiers q v e) = "(" ++ show q ++ v ++ "." ++ show e ++ ")"
  show (Not e)             = "(!" ++ show e ++ ")"
  show (Equal a b)         = "(" ++ show a ++ "=" ++ show b ++ ")"
  show (PVar name)         = name

data Term = Sum Term Term
          | Mul Term Term
          | Inc Term
          | Var String
          | Zero
          deriving (Eq, Ord)

instance Show Term where
  show (Sum a b)  = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Mul a b)  = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Inc e)    = show e ++ "\'"
  show Zero       = "0"
  show (Var name) = name

data Problem = Problem { get :: Expr }

instance Show Problem where
  show (Problem expr) = "|-" ++ show expr
