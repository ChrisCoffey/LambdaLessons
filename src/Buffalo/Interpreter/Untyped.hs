module Buffalo.Interpreter.Untyped (
  interpret
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

--TODO change this to an either & capture common error cases
interpret :: Expr -> (Value, [Step])
interpret exp = evalState (runWriterT (runInterpret emptyScope exp)) (EvalState 0)

nextKey :: String -> EvalScope -> String
nextKey key scope = 
  if ('\'':key) `M.notMember` scope
  then '\'':key
  else nextKey ('\'':key) scope

alphaConversion :: Name -> EvalScope -> EvalScope
alphaConversion (Name n) scope = 
  case M.lookup n scope of 
    Just value -> M.insert newKey value s'
      where
        newKey = nextKey n scope
        s' =  M.delete n scope
    Nothing   -> scope
  
αC = alphaConversion

increaseDepth :: Eval a -> Eval a
increaseDepth m = do
  modify $ \s -> s {depth = depth s + 1 }
  out <- m
  modify $ \s -> s {depth = depth s - 1 }
  return out

pushToOutput :: Expr -> Eval ()
pushToOutput exp = do
  d <- gets depth
  tell [(d, exp)]
  return ()

runInterpret :: EvalScope -> Expr -> Eval Value
runInterpret scope expr =
  case expr of
    Var (Name x) -> 
      pushToOutput expr >> return ( scope M.! x)
    
    Lam (Name x) body -> 
      return (VFunc x body scope)
  
    App a b -> increaseDepth $ do
      x <- runInterpret scope a
      pushToOutput a
      y <- runInterpret scope b
      pushToOutput b
      apply x y

apply :: Value -> Value -> Eval Value
apply (VFunc name body scope) expr = 
  runInterpret (extendScope name expr scope) body

extendScope :: String -> Value -> EvalScope -> EvalScope
extendScope name value = 
  M.insert name value . alphaConversion (Name name) 
      
