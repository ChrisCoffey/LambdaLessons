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

apply :: Expr
apply = Lam (Name "func") (Lam 
                            (Name "arg") 
                            (App (Var (Name "func")) (Var (Name "arg")))
                          )

first :: Expr
first = Lam (Name "x") (Lam
                         (Name "y")
                         (Var (Name "x"))
                       )
second :: Expr
second = Lam (Name "x") (Lam
                         (Name "y")
                         (Var (Name "y"))
                       )

makePair :: Expr
makePair = Lam (Name "first") (Lam 
                                (Name "second")
                                (Lam 
                                  (Name "func")
                                  (App 
                                    (App (Var (Name "func")) (Var (Name "first")))
                                    (Var (Name "second"))
                                  )
                                )
                              )
                              
