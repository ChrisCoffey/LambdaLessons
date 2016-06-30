module LL.Types where

newtype Name = Name String 
  deriving (Show, Eq)

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          deriving (Show, Eq)


