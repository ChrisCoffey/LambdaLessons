module Interps.FlattenAdd where

import Interps.Evaluators
import Interps.Language

import Interps.PushNeg

-- | flattening addition is the process of always pushing additions to the right by taking advantage
-- of associativity. When there is an add immediately followed by another 'Add', these are split up
-- with the left term of the deeper 'Add' pulled to the left outside of its parentheses.
--
-- As you can see, this operation is context-sensitive (nested pattern-matching). so its
-- not inductive on terms. that's not necessarily a bad thing, but it makes checking that it
-- doesn't terminate a bit more difficult.
flattenAdd :: Exp -> Exp
flattenAdd (Add (Add l r) r') = flattenAdd $ Add l (Add r r')
flattenAdd (Add l r) = Add l $ flattenAdd r
flattenAdd exp = exp

optimizeExp :: Exp -> Exp
optimizeExp = flattenAdd . pushNeg

-- | The nested pattern-matching in 'flattenAdd' shows that we care about addition terms when they
-- are the left-hand child of an addition term. Therefore, we can factor this out into a context
-- type and pass that around using the same function instance technique used in pushing negation.
data CtxA exp = LH exp | NonLH

-- | This definiton shows the trickiness of passing context through more clearly than the 'pushNeg'
-- example did. The main complexity comes in remembering to inject 'add' for literals based on the
-- provided context.
--
-- the final term, 'add', is the following equality: CtxA (Add l r) ~ Add l (CtxA r)
instance ArithLang exp => ArithLang (CtxA exp -> exp) where
    lit n NonLH = lit n
    lit n (LH e) = add (lit n) e
    neg e NonLH = neg $ e NonLH
    neg e (LH e') = add (neg $ e NonLH) e'
    add l r ctx = l . LH $ r ctx

-- | TODO implement multiplication
