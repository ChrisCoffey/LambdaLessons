module Buffalo.Interpreter.Untyped (

) where

import Buffalo.Types

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map             as M


type Step           = (Int, Expr)
type EvalScope      = M.Map String Value
type Eval a         = WriterT [Step] (State EvalState) a
newtype EvalState   = EvalState {depth :: Int } deriving Show

data Value = VInt Integer
           | VBool Bool
           | VFunc String Expr EvalScope
           deriving Show

emptyScope :: EvalScope
emptyScope = M.empty

interpret :: Expr -> Either String (Value, [Step])
interpret exp = evalState (runWriterT (runInterpret emptyScope exp)) (EvalState 0)

runInterpret :: EvalScope -> Expr -> Eval Value
runInterpret scope expr = 
