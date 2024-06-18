module SeqInterpreter where

import Abs
import qualified Interpreter as Interp
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.List



eval :: Phrase -> Interp.JavaState ()
eval (PExpr e) = Interp.exec $ Print e
eval (PStmt s) = Interp.exec s
eval (PSeq p1 p2) = eval p1 >> eval p2
eval (PClass c) = Interp.declareClass c
eval (PVar v) = Interp.declareVariables [v] >>= \newenv -> modify (\ctx -> ctx { Interp.env = M.union newenv (Interp.env ctx) })
eval (PMethod m) = do
    env' <- Interp.declareMethod m
    modify (\ctx -> ctx { Interp.env = M.union env' (Interp.env ctx) })
eval (PMethodCall name args) = do
    ctx <- get
    case M.lookup name (Interp.env ctx) of
        Just (Interp.Closure f) -> Interp.evalActuals args >>= \actuals -> void $ f (Interp.listLitAppend (Interp.ListLit [Interp.ObjInstance 0 "" M.empty []]) actuals)
        _ -> get >>= \ctx -> error $ "Not a closure.: " ++ name ++ " Env: " ++ show (Interp.env ctx)



runPhrase :: Phrase -> Interp.Context -> IO (Maybe Interp.Context, [String])
runPhrase p c = do
    let cnew = execState (eval p) c
    return (Just cnew, Interp.out cnew \\ Interp.out c)