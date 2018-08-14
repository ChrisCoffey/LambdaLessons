{
module Lang.Booleans (
    Term(..),
    AlexPosn(..),
    alexScanTokens,
    token_posn
    ) where
}

%wrapper "posn"

$zero = 0
$alpha = [a-zA-Z]

t :-
    $white+     ;
    $zero       {makeT (\p s -> Zero p)}
    $alpha+     {makeT (\p s -> T p s)}

{
data Term
    = T AlexPosn String
    | Zero AlexPosn
    deriving (Eq, Show)

makeT f p s = f p s

token_posn (Zero p) = p
token_posn (T p _) = p
}

