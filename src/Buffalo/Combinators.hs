module Buffalo.Combinators where

import Buffalo.Types

i :: Expr
i = Lam (Name "x") $ Var (Name "x")

isI :: Expr -> Bool
isI (Lam (Name x) (Var (Name y))) 
  | x == y    = True
  | otherwise = False
isI _     = False

selfApply :: Expr
selfApply = Lam (Name "s") $ App (Var (Name "s")) (Var (Name "s"))

isSelfApply :: Expr -> Bool
isSelfApply (Lam (Name x) (App (Var (Name y)) (Var (Name z)) ))
  | x == y && x == z  = True
  | otherwise         = False
isSelfApply _         = False


