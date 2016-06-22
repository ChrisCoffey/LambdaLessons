module Buffalo.Types where

newtype Name = Name String 
  deriving Show

data Expr = Var Name
          | App Expr Expr
          | Lam Name Expr
          deriving Show

