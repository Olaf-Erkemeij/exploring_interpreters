{-# LANGUAGE DeriveGeneric #-}
module Interpreter where

import Abs ( Expr(..) )
import qualified Data.Map as M
import Control.Monad.State
import Data.List ( (\\) )
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.Binary (Binary)

data LeafVal = IntLeaf Int | BoolLeaf Bool | StringLeaf String
data Func = Head [String] Func
  | Leaf String
  | Monop String Func
  | Binop String Func Func
  | Ternop String Func Func Func
  | ExprFunc [String] Expr
  deriving (Eq, Show, Generic)

instance NFData Func
instance ToJSON Func where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Func
instance Binary Func

plus :: Func
plus = Head ["a", "b"] (Binop "+" (Leaf "a") (Leaf "b"))

minus :: Func
minus = Head ["a", "b"] (Binop "-" (Leaf "a") (Leaf "b"))

times :: Func
times = Head ["a", "b"] (Binop "*" (Leaf "a") (Leaf "b"))

divide :: Func
divide = Head ["a", "b"] (Binop "/" (Leaf "a") (Leaf "b"))

lessThan :: Func
lessThan = Head ["a", "b"] (Binop "<" (Leaf "a") (Leaf "b"))

ifFunc :: Func
ifFunc = Head ["cond", "a", "b"] (Ternop "if" (Leaf "cond") (Leaf "a") (Leaf "b"))

display :: Func
display = Head ["a"] (Monop "display" (Leaf "a"))

modifyEnv :: Env -> [(String, Value)] -> EvalM Value -> EvalM Value
modifyEnv baseEnv bindings comp = do
  oldCtx <- get
  let newEnv = M.union (M.fromList bindings) baseEnv
  put oldCtx { env = newEnv }
  result <- comp
  newOut <- gets out
  put oldCtx { out = newOut }
  return result

evaluate :: Func -> [Value] -> EvalM Value
evaluate (Head args f) vals = do
  when (length args /= length vals) $ error "Wrong number of arguments"
  curEnv <- gets env
  modifyEnv curEnv (zip args vals) (evaluate' f)
evaluate (ExprFunc args body) vals = do
  when (length args /= length vals) $ error "Wrong number of arguments"
  curEnv <- gets env
  modifyEnv curEnv (zip args vals) (eval body)
evaluate _ _ = error "Invalid function"

evaluate' :: Func -> EvalM Value
evaluate' (Leaf s) = do
  curEnv <- gets env
  case M.lookup s curEnv of
    Just val -> return val
    Nothing  -> error ("Undefined symbol: " ++ s)
evaluate' (Binop op a b) = do
  a' <- evaluate' a
  b' <- evaluate' b
  case op of
    "+" -> numFunc sum [a', b']
    "*" -> numFunc product [a', b']
    "-" -> numFunc (\[x, y] -> x - y) [a', b']
    "/" -> numFunc (\[x, y] -> x `div` y) [a', b']
    "<" -> boolFunc (<) [a', b']
    _ -> error "Invalid operator"
evaluate' (Monop op a) = do
  a' <- evaluate' a
  case op of
    "display" -> do
      modify $ \ctx -> ctx { out = out ctx ++ [show a' ++ "\n"] }
      return Void
    "not" -> BoolVal . not <$> fromBool a'
    _ -> error "Invalid operator"
evaluate' (Ternop op cond a b) = case op of
  "if" -> do
    cond' <- evaluate' cond
    case cond' of
      BoolVal True -> evaluate' a
      BoolVal False -> evaluate' b
      _ -> error "Condition in if must be a boolean"
  _ -> error "Invalid operator"
evaluate' _ = error "Invalid function"

data Value
  = Void
  | BoolVal Bool
  | IntVal Int
  | StringVal String
  | FunVal Func
  | ListValue [Value]
  deriving (Eq, Generic)

instance NFData Value
instance ToJSON Value where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Value
instance Binary Value

instance Show Value where
  show Void = "<void>"
  show (BoolVal b) = show b
  show (IntVal i) = show i
  show (StringVal s) = show s
  show (FunVal _) = "<function>"
  show (ListValue xs) = "(" ++ unwords (map show xs) ++ ")"

prettyPrint :: Value -> String
prettyPrint (ListValue xs) = "(" ++ unwords (map prettyPrint xs) ++ ")\n"
prettyPrint Void = ""
prettyPrint (FunVal _) = ""
prettyPrint x = show x ++ "\n"

prettyPrintExpr :: Expr -> String
prettyPrintExpr (Integer i) = show i
prettyPrintExpr (Boolean b) = if b then "true" else "false"
prettyPrintExpr (String s) = show s
prettyPrintExpr (Symbol s) = s
prettyPrintExpr (Lambda args body) = "(lambda (" ++ unwords args ++ ") " ++ prettyPrintExpr body ++ ")"
prettyPrintExpr (List xs) = "(" ++ unwords (map prettyPrintExpr xs) ++ ")"

type Env = M.Map String Value
type EvalM = State Context

data Context = Context
  { env :: Env
  , out :: [String]
  }
  deriving (Eq, Show, Generic)

instance NFData Context
instance ToJSON Context where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Context
instance Binary Context

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
    [ ("+", FunVal plus)
    , ("*", FunVal times)
    , ("-", FunVal minus)
    , ("/", FunVal divide)
    , ("<", FunVal lessThan)
    , ("if", FunVal ifFunc)
    , ("display", FunVal display)
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
eval (Lambda args body) = return (FunVal (ExprFunc args body))
eval (List (func : args)) = do
  f <- eval func
  argVals <- mapM eval args
  case f of
    FunVal f' -> evaluate f' argVals
    _ -> error "Not a function"
eval expr@(List _) = error $ "Unimplemented list operation: " ++ show expr

runExpr :: Expr -> Context -> IO (Maybe Context, [String])
runExpr expr ctx = do
  let (v, ctx') = runState (eval expr) ctx
  return (Just ctx', (out ctx' \\ out ctx) ++ [prettyPrint v])
