module LL.Combinators where

import LL.Types

-- λx.x
i :: Expr
i = Lam (Name "x") $ Var (Name "x")

isI :: Expr -> Bool
isI (Lam (Name x) (Var (Name y))) 
  | x == y    = True
  | otherwise = False
isI _     = False

-- λs.(s s)
selfApply :: Expr
selfApply = Lam (Name "s") $ App (Var (Name "s")) (Var (Name "s"))

isSelfApply :: Expr -> Bool
isSelfApply (Lam (Name x) (App (Var (Name y)) (Var (Name z)) ))
  | x == y && x == z  = True
  | otherwise         = False
isSelfApply _         = False

-- λfunc.λarg.(func arg)
apply :: Expr
apply = Lam (Name "func") (Lam 
                            (Name "arg") 
                            (App (Var (Name "func")) (Var (Name "arg")))
                          )

-- λx.λy.x
first :: Expr
first = Lam (Name "x") (Lam
                         (Name "y")
                         (Var (Name "x"))
                       )
isFirst = (== first)
-- λx.λy.y
second :: Expr
second = Lam (Name "x") (Lam
                         (Name "y")
                         (Var (Name "y"))
                       )
isSecond = (== second)

-- λfirst.λsecond.λfunc.((func first) second)
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

true = first
false = second

isBool :: Expr -> Bool
isBool expr = expr == true || expr == false 

--λn.λs.((s false) n) 
succ :: Expr
succ = Lam (Name "n") (Lam (Name "s") (App (App (Var (Name "s")) false) (Var (Name "n"))))

zero :: Expr
zero = i

