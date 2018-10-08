module Interps.Language where

-- | An initial embedding langauage implemetnation of simple arithmetic expressions
data Exp
    = Lit Int
    | Neg Exp
    | Add Exp Exp
    deriving (Show)

term1 :: Exp
term1 = Add (Lit 8)
            (Neg
                (Add (Lit 1) (Lit 2)))

-- | An alternative embedding that uses Haskell directy. There is a representational type
-- and functions that handle each of the three terms in the language.
type Repr = Int
lit1 :: Int -> Repr
lit1 = id

neg1 :: Repr -> Repr
neg1 n = -n

add1 :: Repr -> Repr -> Repr
add1 l r = l + r

-- | This embedding is called a final embedding. It is meta-circular, meaning that each term in the
-- target laugnage is defined using a corresponding term/function in the host language. In this case, our
-- target langauge is 'Repr' and the host is Haskell.
term1' :: Repr
term1' = add1 (lit1 8) (neg1 (add1 ( lit1 1 ) (lit1 2)))

-- | Unfortunately, 'term1'' has a fixed 'Repr' to 'Int', which means its impossible to implement a pretty-printer
-- using the current formulation. However, if the language is moved from raw functions into a typeclass then
-- this becomes possible.
--
-- In the example below, the meaning of the expression, whatever 'repr' is set to, is computed from the meaning of
-- the constituent parts. This is typical of denotational semantics, which may be used to great effect when reasoning
-- about a final embedding.
--
-- This class represents the syntax of our embedded 'Expr' language.
class ArithLang repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

term1f :: ArithLang repr => repr
term1f = add (lit 8) (neg (add (lit 1) (lit 2)))

-- | To add Multiplication to 'Expr', we need to find some means of dealing with the expression problem. For the
-- initial embedding, this is pretty painful. Product types are one means of solving it, but they're clumsy once there
-- are a number of extensions involved.
data Expr2
    =
    Lit2 Int
    | Neg2 Expr2
    | Add2 Expr2 Expr2
    | Mul Expr2 Expr2

-- | All of the terms in 'Exp' could be rewritten in 'Expr2', but I'm going to pass on that for now.

class MulLang repr where
    mul :: repr -> repr -> repr

term1FM :: (ArithLang repr, MulLang repr) => repr
term1FM = add (lit 7) (neg (mul (lit 1) (lit 2)))

term2FM :: (ArithLang repr, MulLang repr) => repr
term2FM = mul (lit 7) term1f
