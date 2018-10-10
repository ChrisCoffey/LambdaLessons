module Interps.PushNeg where

import Interps.Language
import Interps.Evaluators

-- | Pushing negation down to the leaves/ individual integer litterals is a common and effecitve optimization
-- technique for arithmetic expressions. Performing this optimization is context-sensitive though, as illustrated
-- in the following pattern matching code.
--
-- Its pretty unobvious how 'pushNeg' actually reaches termination (although it does, and can therefore be modeled
-- as a 'fold'). It relies on nested pattern matching (i.e. contexte-sensitivity) on the terms, then in certain
-- contexts pushing that 'Neg' down towards the leaves.
pushNeg :: Exp -> Exp
pushNeg (Lit n) = Lit n
pushNeg e@(Neg (Lit n)) = e
pushNeg (Neg (Neg e)) = pushNeg e
pushNeg (Neg (Add l r)) = Add (pushNeg (Neg l)) (pushNeg (Neg r))
pushNeg (Add l r) = Add (pushNeg l) (pushNeg r)

-- | as you can tell from the type signature, the initial approach to pushing negation doesn't somehow save
-- us from all of the issues presented by initial embeddings (namely expression problem).
pushedInitial :: Exp
pushedInitial = pushNeg term1

-- | For the final embedding we can make the context explicit and pass it through all of the terms. This allows
-- us to effectively pattern match on the important bits without breaking out of our final embedding. The whole
-- thing hinges on the ability to both make the context an explicit type and provide an instance of 'ArithLang'
-- that supports this additional context.
data Ctx = P | N

-- | The first time you look at this instance it can be a bit mind-bending. Essentially, we've added an additional
-- parameter to all of the embedding's terms. This works because of function currying, which means that while the
-- initial terms are each single-argument, if we define an instance that takes an additional argument then we're
-- able to actually pattern match on that additional argument in the instance's definition. I.e. Single argument
-- functions that return single arguent functions are equivalent to two argument functions.
instance ArithLang a => ArithLang (Ctx -> a) where
    lit n P = lit n
    lit n N = neg $ lit n
    neg t N = t P
    neg t P = t N
    add l r ctx = add (l ctx) (r ctx)

pushNegF :: (Ctx -> a) -> a
pushNegF t = t P

-- | Unlike the intial embedding, the final embedding's 'pushNegF' implementation is extensible by default. Simply
-- by defining an instance for any new language terms and we're able to use them right away. The following
-- instance and extension illustate how this works.
instance MulLang a => MulLang (Ctx -> a) where
    mul l r P = mul (l P) (r P)
    mul l r N = mul (l N) (r N)

