module Typed.HigherOrder where

import Data.Monoid ((<>))

-- | The key
class PCFSemantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

th1 :: PCFSemantics repr => repr Int
th1 =  add (int 1) (int 2)
th2 :: PCFSemantics repr => repr (Int -> Int)
th2 = lam (\x -> add x x)
th3 :: PCFSemantics repr => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x $ int 1) (int 2))

-- | Now that Haskell is tracking variables for us, there's no need for 'PCFR' to do anything beyond
-- expose the result type. I.e. 'PCFR' is just a name for an interpreter that wraps Haskell values. By itself
-- it is equivalent to identity, so only provides a name for us to write our meta-circular interpreter instance.
newtype PCFR a = PCFR { unPCFR :: a }

instance PCFSemantics PCFR where
    int n = PCFR n
    add l r = PCFR $ unPCFR l + unPCFR r

    lam f = PCFR $ unPCFR . f . PCFR -- Wrap the incoming a in a PCFR, apply it to 'f', then unwrap the resulting 'PCFR b'.
    app f x = PCFR $ (unPCFR f) (unPCFR x)

evalO :: PCFR a -> a
evalO = unPCFR

newtype PCFS a = PCFS {unPCFS :: Int -> String }

instance PCFSemantics PCFS  where
    int n = PCFS . const $ show n
    add l r = PCFS $ \x ->
        "("<> show (unPCFS l x)<> " + " <> show (unPCFS r x) <>")"

    lam f = PCFS $ \n -> let
        x = "x"<> show n
        fs = unPCFS (f (PCFS $ const x)) (n+1)
        in "(\\"<> x <> " -> " <> fs <> ")"

    app f x = PCFS $ \n ->
        "("<> unPCFS f n <>" "<> unPCFS x n<> ")"

viewO :: PCFS a -> String
viewO exp = unPCFS exp 0

-- | Extending the language with new terms and syntax
class MulSyn repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSyn repr where
    tf :: Bool -> repr Bool
    bLeq :: repr Int -> repr Int -> repr Bool
    bIf :: repr Bool -> repr a -> repr a -> repr a

-- | This allows you to discriminate between call-by-value and call-by-name semantics. By avoiding 'fix',
-- you'll use the default Haskell convention, which is call-by-name. However, 'fix' can introduce enough
-- recursion to fully evaluate a value, which converts it to call-by-value.
--
-- TODO look into some worked examples of how the 'fix' combinator actually works.
-- Turns out that this is the Y combinator
class FixSyn repr where
    fix :: (repr a -> repr a) -> repr a

tpow :: (PCFSemantics repr, MulSyn repr, BoolSyn repr, FixSyn repr) =>
    repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\self -> lam (\n ->
    bIf (bLeq n (int 0))
        (int 1)
        (mul x (app self (add n (int (-1)))))
    )))

toThe7th :: (PCFSemantics repr, MulSyn repr, BoolSyn repr, FixSyn repr) =>
    repr (Int -> Int)
toThe7th = lam (\x -> app (app tpow x) (int 7))

twoToThe7th ::(PCFSemantics repr, MulSyn repr, BoolSyn repr, FixSyn repr) =>
    repr Int
twoToThe7th = app toThe7th (int 2)

instance MulSyn PCFR where
    mul l r = PCFR $ unPCFR l * unPCFR r

instance BoolSyn PCFR where
    tf = PCFR
    bLeq l r = PCFR $ unPCFR l <= unPCFR r
    bIf p t f = if unPCFR p then t else f


instance FixSyn PCFR where
    fix f = PCFR $ fx (unPCFR . f . PCFR)
        where fx f = f (fx f)

idO :: repr a -> repr a
idO = id

testTerm :: (PCFSemantics repr, FixSyn repr) => repr Int
testTerm = lam (\x -> (int 5)) `app` (fix idO)
