module Typed.Language where

import Data.Monoid ((<>))

-- | A typed, albeit simply, lambda calculus. Integers may be expressed using "de Brujin Indexes",
-- aka the number of lambda binders between a variable's occurance & its binder. This allows you to
-- ignore alpha-conversions (renamings) and just use a variable's index.
data SVar = VZero | VSome SVar
    deriving (Show)
data SLambda
    = SVar SVar
    | SB Bool
    | SApp SLambda SLambda
    | SAbst SLambda
    deriving (Show)

lId :: SLambda
lId = SAbst (SVar VZero)

ti1 = SApp lId $ SB True

data U = UB Bool | UF (U -> U)

eval :: [U] -> SLambda -> U
eval env (SVar v) = fromEnv v env
eval env (SB b) = UB b
eval env (SAbst exp) = UF $ \x -> eval (x:env) exp
eval env (SApp exp arg) = case eval env exp of UF f -> f (eval env arg)

fromEnv :: SVar -> [U] -> U
fromEnv VZero (x:_) = x
fromEnv (VSome var) (_:rest) = fromEnv var rest

-- | Unfortunatley, the above definition of 'eval' is partial and because its possible to define
-- more terms in the object languge than can be safely evaluated, Haskell's typesystem cannot
-- express it properly. That's only with the current forumulation, not necessarily for all possible
-- implementations in Haskell. Even in the presence of a typechecking function
-- `Exp -> Either ErrorMsg Exp`, we don't gain any additional type-safety. This means the runtime
-- type tags 'UB' and 'UF' still need to flow through the evaluator. Its true that a runtime
-- typechecker will make 'eval' total (in behavior if not signature), but we pay the mental and
-- computational overhead of passing around the type tags.
--
-- A "tight" embedding is one in which it is not possible to pass invalid terms to an evaluator. The
-- embedding used above is not "tight".

-- To tighten the embedding we can use GADTs.This lets us express constraints on the size of each
-- constructor. Basically, ill-typed expressions simply cannot exist if we use GADTs properly.
--
-- Additionally, the GADT implementation below is tagless! Becasue we can push the constraints into
-- the GADT, there's no reason to define a Univers type 'U', which reduces the runtime overhead and
-- complexity of these functions.
data Lam env a where
    B :: Bool -> Lam env Bool
    V :: LVar env a -> Lam env a
    L :: Lam (a,env) b -> Lam env (a -> b)
    A :: Lam env (a -> b) -> Lam env a -> Lam env b

-- | This is actually a really simple variationon on an HList
data LVar env a where
    LZ :: LVar (a,env) a
    LS :: LVar env a -> LVar (b,env) a

fromEnvSafe :: LVar env a -> env -> a
fromEnvSafe LZ (a,_) = a
fromEnvSafe (LS next) (_, rest) = fromEnvSafe next rest

-- | Notice the lack of any 'U' or type tags. The very definition of our language with GADTs lets
-- the compiler prove that terms are valid.
evalSafe :: env -> Lam env a -> a
evalSafe env (B b)  = b
evalSafe env (V var) = fromEnvSafe var env
evalSafe env (L f) = \x -> evalSafe (x,env) f
evalSafe env (A f a) = evalSafe env f (evalSafe env a)

tis1 = A (L $ V LZ) (B True)

--
-- Typed Tagless Final encoding of simply-typed lambda calculus
--

-- | Now lets see what this language looks like if we abstract over the result type. In the previous
-- initial embedding it was fixed to 'Bool'. A change from 'Bool' to 'Integer' or 'String' would
-- have been pretty difficult to accomplish. But, perhaps we can pull from our earlier work with
-- unityped final interpreters and provie a typeclass that allows multiple interpretations.
--
-- We'll probably need to use the same duplciate trick to pass on copies of the initial value to
-- different interpreters, but that's fine.
class LambdaSemantics rep where
    int :: Int -> rep env Int
    add :: rep env Int -> rep env Int -> rep env Int

    z :: rep (v,env) v
    s :: rep env c -> rep (any, env) c
    app :: rep env (a -> b) -> rep env a -> rep env b
    lam :: rep (a,env) b -> rep env (a -> b)

td1 :: LambdaSemantics rep => rep env Int
td1 =  add (int 1) (int 4)
-- td2Open is an open term, meaning that it has too many free variables, and therefore may not be interpreted from
-- an empty environment
td2Open :: LambdaSemantics rep => rep (Int,env) (Int -> Int)
td2Open = lam (add z (s z))
td3 :: LambdaSemantics rep => rep env ((Int -> Int) -> Int)
td3 = lam (add (app z (int 1)) (int 2))

-- | This type is pretty mind-bending, but essentialy wraps a 'Reader'. Given some environment type
-- 'env', then it is guaranteed to produce the result type 'a'. The type for 'runReader' is: 'Reader r a -> r -> a'
-- So, if we partially apply the 'Reader r a' to 'runReader' we're left with the same signature as 'unR',
-- 'r -> a'.
--
-- Therefore, we can embed partially applied 'runReader' calls into this type (at least the moral equivalent of
-- a 'runReader'). This works to prove that the embedding of the object language into Haskell is valid. After
-- all, if it were invalid then the typechecker would reject an attempt to construct the 'R'.
newtype R env a = R {unR :: env -> a}

-- | The actual embedding of our simply typed lambda calculus into Haskell. As mentioned above in the comment on
-- 'R', the right hand side serves as a proof that the type "soundness" of our lambda calculus can be reduced to
-- normal type "soundness" in Haskell (which we have confidence in).
--
-- Therefore, if we can produce a well-typed instance of 'LambdaSemantics' then we can have confidence that the
-- lambda calculus itself is well-typed.
instance LambdaSemantics R where
    int n = R $ const n
    add l r = R $ \env -> (unR l env) + (unR r env)

    z = R $ \(a,_) -> a
    s env = R $ \(_,rest) -> unR env rest

    app l r = R $ \env -> (unR l env) (unR r env)
    lam f = R $ \env -> \x -> unR f (x,env)

evalS :: R () a -> a
evalS e = unR e ()

-- | An alternative that produces 'String' representation of lambda terms. This looks identical to 'R' except that
-- the result type has been fixed to 'String'.
newtype S env a = S {unS :: Int -> String}

instance LambdaSemantics S where
    int n = S . const $ show n
    add l r = S $ \env -> "("<>unS l env <> " + " <>unS r env<>")"

    z = S $ \env -> "x"<> show (env - 1) -- Print the nth variable
    s v = S $ \env -> unS v (env - 1) -- search for the nth variable

    app l r = S $ \env -> "("<>unS l env <>" "<>unS r env<>")"
    lam f = S $ \n ->
        let x = "x"<>show n -- Introducing x-sub n, or the nth variable into the expression
        in "(\\"<>x<>" -> "<> unS f (n+1)<> ")" -- wrap the underlying function in a new variable, then increment the var counter

viewS :: S env a -> String
viewS e = unS e 0
