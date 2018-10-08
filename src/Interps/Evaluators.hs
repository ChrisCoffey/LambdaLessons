module Interps.Evaluators where

import Interps.Language (Exp(..), term1, term1f, ArithLang(..), MulLang(..))

import Data.Monoid((<>))
import Data.Text as T

-- | Interpret an 'Exp' by case analysis, i.e. pattern-matching
eval :: Exp -> Int
eval (Lit n) = n
eval (Neg exp) = - eval exp
eval (Add l r) = eval l + eval r

view :: Exp -> T.Text
view (Lit n) = T.pack $ show n
view (Neg exp) = "(-"<> view exp <>")"
view (Add l r) = "("<> view l <>" + "<> view r<>")"

-- | Interpret an 'Exp' that has been embedded into 'ArithLang'
instance ArithLang Int where
    lit = id
    neg n = -n
    add l r = l + r

-- | This simply sleects which of the evaluators we want to use on the finally embedded version of 'Exp'
-- In this particualr case, it has been fixed to 'Int', and selects the corresponding instance
evalF :: Int -> Int
evalF = id

instance ArithLang T.Text where
    lit = T.pack . show
    neg n = "(- " <> n <> " )"
    add l r = "( "<>l<>" + "<>r<>" )"

viewF :: T.Text -> T.Text
viewF = id

-- | Extending the evaluation of 'Expr'. It's important to point out that because all 'evalF' and 'viewF' do
-- is fix the type for the final embedding, there's no reason to change their implementations as the language
-- gets extended.
instance MulLang Int where
    mul = (*)

instance MulLang T.Text where
    mul l r = "( "<>l<>" x "<>r<>" )"
