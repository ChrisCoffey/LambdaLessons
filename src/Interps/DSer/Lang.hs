module Interps.DSer.Lang where

import Control.Monad (liftM, liftM2)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Interps.Language (ArithLang(..), term1f)
import Interps.Evaluators (viewF, evalF)
-- | The deserialiation problem is related to the expression problem. Essentially, instead of worrying about
-- how to extend a known language that can be easily expressed in code, we're concerned with transforming
-- multiple stored versions of the some language into usable terms.

-- | The initial embedding of a language to play with the deserialization problem
data Tree
    = Leaf String
    | Node String [Tree]
    deriving (Eq, Read, Show) -- Read & Show are included to provide "easy" serialization

instance ArithLang Tree where
    lit n = Node "Lit" [Leaf $ show n]
    neg exp = Node "Neg" [exp]
    add l r = Node "Add" [l,r]

toTree :: Tree -> Tree
toTree = id

safeRead :: Read a => String -> Either String a
safeRead str = case reads str of
    [(x, "")] -> Right x
    _ -> Left $ "Read.error: " <> str

-- | Tranform a serialized 'Expr' into some other representation.
fromTree :: ArithLang repr => Tree -> Either String repr
fromTree (Node "Lit" [Leaf n]) = liftM lit $ safeRead n
fromTree (Node "Neg" [exp]) = liftM neg $ fromTree exp
fromTree (Node "Add" [l,r]) = liftM2 add (fromTree l) (fromTree r)
fromTree wrong = Left $ "Invalid Tree: " <> show wrong


-- | This works for a single evaluator, but we lose the ability to use multiple interpreters on the
-- term produced via 'fromTree'. The initial 'repr' is still polymorphic, but because of the
-- pattern-matching the 'repr' becomes fixed & we're no longer able to evaluate it into additional
-- types.
tf1' :: (Show a, ArithLang t) => (t -> a) -> IO ()
tf1' eval = let
    tf1' = fromTree (toTree term1f)
    in case tf1' of
        Left e -> putStrLn $ "Error: " <> e
        Right res -> print $ eval res

-- | Using an existential type is an option, but that takes away extensibility. Therefore, we need something
-- different to atain a solution to both the expression & deserialization problems.
--
-- By introducing a type that duplicates 'repr', we're able to use multiple representations within the same
-- deserialization function. This works by allowing us to interpret the value, but also pass on a copy to the
-- next interpreter. On the downside, it builds up an ungainly type of nested pairs.
instance (ArithLang repr, ArithLang repr') => ArithLang (repr, repr') where
    lit x = (lit x, lit x)
    neg (a,b) = (neg a, neg b)
    add (l, r) (l', r') = (add l l', add r r')

duplicate :: (ArithLang a, ArithLang b) => (a,b) -> (a,b)
duplicate = id

checkConsume f (Left e) = putStrLn $ "Error: " <> e
checkConsume f (Right x) = f x

-- | This applys an interpreter to 'x' then passes along a copy to any downstream interpreters.
duplicateConsume :: (Show a, ArithLang repr, ArithLang repr') =>
    (repr -> a) -- ^ Tranform to prepare for some output
    -> (repr, repr') -- ^ The input pair
    -> IO repr' -- ^ Returning the second component of the pair
duplicateConsume eval x = print (eval x1) >> pure x2
    where (x1, x2) = duplicate x

-- | All upstream interpreters ('evalF' and 'viewF') use 'duplicateConsume' to create a copy of the input
-- statement and pass it along. The last interpreter applied ('toTree' in this case) does not need to pass
-- a copy, so it is simply applied.
evalThree :: (Int, (T.Text, Tree)) -> IO ()
evalThree x = print . toTree =<< duplicateConsume viewF =<< duplicateConsume evalF x

tf1'Triple = checkConsume evalThree . fromTree $ toTree term1f

-- |
fromTreeExt :: ArithLang repr =>
    (Tree -> Either String repr)
    -> (Tree -> Either String repr)
fromTreeExt _ (Node "Lit" [Leaf n]) = lit <$> safeRead n
fromTreeExt self (Node "Neg" [e]) = neg <$> self e
fromTreeExt self (Node "Add" [l,r]) = liftM2 add (self l) (self r)
fromTreeExt _ statement = Left $ "Error: Invalid Tree" <> show statement

-- | As defined in Data.Function. This is pretty mind-bending
fix :: (a -> a) -> a
fix f = f (fix f)

fromTree' :: ArithLang repr =>
    Tree ->
    Either String repr
fromTree' = fix fromTreeExt
