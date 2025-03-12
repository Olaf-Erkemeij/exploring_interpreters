{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Abs
import qualified Data.Map as M
import Control.Monad.State
import Data.List

data Value
  = Void
  | BoolVal Bool
  | IntVal Int
  | StringVal String
  | FunVal ([Value] -> EvalM Value)
  | ListValue [Value]
  | LambdaVal [String] Expr Context

instance Show Value where
  show Void = "<void>"
  show (BoolVal b) = show b
  show (IntVal i) = show i
  show (StringVal s) = show s
  show (FunVal _) = "<function>"
  show (ListValue xs) = "(" ++ unwords (map show xs) ++ ")"
  show (LambdaVal args _ _) = "<lambda: " ++ unwords args ++ ">"

type Env = M.Map String Value
type EvalM = State Context

data Context = Context
  { env :: Env
  , out :: [String]
  }

fromInt :: Value -> EvalM Int
fromInt (IntVal i) = return i
fromInt _ = error "Invalid arguments to function"

fromBool :: Value -> EvalM Bool
fromBool (BoolVal b) = return b
fromBool _ = error "Invalid arguments to function"

numFunc :: ([Int] -> Int) -> [Value] -> EvalM Value
numFunc op args = IntVal . op <$> mapM fromInt args

boolFunc :: (Int -> Int -> Bool) -> [Value] -> EvalM Value
boolFunc op [IntVal x, IntVal y] = return (BoolVal (op x y))
boolFunc _ _ = error "Invalid arguments to function"

builtins :: Env
builtins = M.fromList
    [ ("+", FunVal $ numFunc sum)
    , ("-", FunVal $ numFunc $ \case [x] -> -x; xs -> foldl1 (-) xs)
    , ("*", FunVal $ numFunc product)
    , ("<", FunVal $ boolFunc (<))
    , (">", FunVal $ boolFunc (>))
    , ("<=", FunVal $ boolFunc (<=))
    , (">=", FunVal $ boolFunc (>=))
    , ("=", FunVal $ boolFunc (==))
    , ("if", FunVal $ \case
        [BoolVal b, t, f] -> return (if b then t else f)
        _ -> error "Invalid arguments to function")
    , ("list", FunVal $ return . ListValue)
    , ("cons", FunVal $ \case
        [x, ListValue xs] -> return (ListValue (x : xs))
        _ -> error "Invalid arguments to function")
    , ("car", FunVal $ \case
        [ListValue (x : _)] -> return x
        _ -> error "Invalid arguments to function")
    , ("cdr", FunVal $ \case
        [ListValue (_ : xs)] -> return (ListValue xs)
        _ -> error "Invalid arguments to function")
    , ("display", FunVal $ \case
        [x] -> do
          modify $ \ctx -> ctx { out = out ctx ++ [show x ++ "\n"] }
          return Void
        _ -> error "Invalid arguments to function")
    ]

initialContext :: Context
initialContext = Context
  { env = builtins
  , out = []
  }

eval :: Expr -> EvalM Value
eval (Integer i) = return (IntVal i)
eval (Boolean b) = return (BoolVal b)
eval (String s) = return (StringVal s)
eval (Symbol s) = do
  curEnv <- gets env
  case M.lookup s curEnv of
    Just val -> return val
    Nothing  -> error ("Undefined symbol: " ++ s)
eval (List [Symbol "define", Symbol var, expr]) = do
  val <- eval expr
  modify $ \ctx -> ctx { env = M.insert var val (env ctx) }
  return val
eval (Lambda args body) = do
  gets (LambdaVal args body)
eval (List (func : args)) = do
  f <- eval func
  argVals <- mapM eval args
  apply f argVals
eval expr@(List _) = error $ "Unimplemented list operation: " ++ show expr

apply :: Value -> [Value] -> EvalM Value
apply (FunVal f) args = f args
apply (LambdaVal params body closure) args = do
  when (length params /= length args) $ error "Wrong number of arguments"

  let closureEnv = env closure
  let newEnv = M.union (M.fromList (zip params args)) closureEnv

  oldEnv <- get
  put $ oldEnv { env = newEnv }
  result <- eval body
  put oldEnv
  return result

apply _ _ = error "Not a function"

runExpr :: Expr -> Context -> IO (Maybe Context, [String])
runExpr expr ctx = do
  let (_, ctx') = runState (eval expr) ctx
  return (Just ctx', out ctx' \\ out ctx)