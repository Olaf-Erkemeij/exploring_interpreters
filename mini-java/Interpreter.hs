{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Interpreter where

import Abs
import Control.DeepSeq
import Control.Monad.Reader (MonadReader (local))
import Control.Monad.State.Lazy
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Data.Hashable
import GHC.IO.Handle.Text (commitBuffer')
import GHC.RTS.Flags (CCFlags (msecsPerTick))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, defaultOptions, genericToEncoding, toEncoding)
import Data.Binary (Binary)

-- import GHC.DataSize

type Ref = Int

type Env = M.Map String Val

type Store = M.Map Ref Val

data Val
  = BoolLit Bool
  | IntLit Int
  | Vec [Int]
  | MethodClosure MethodDecl Env
  | ObjInstance Ref String Env [Val]
  | ClassInstance ClassDecl Env [String]
  | Null
  | Ref Ref
  | ListLit [Val]
  deriving (Generic, Eq)

instance ToJSON Val where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Val
instance Binary Val

instance Show Val where
  show (BoolLit b) = show b
  show (IntLit i) = show i
  show (Vec ints) = show ints
  show (ObjInstance r s env vals) = "Object[id=" ++ show r ++ ", class=" ++ show s ++ ", env=" ++ show env ++ ", parents=" ++ show vals ++ "]"
  show (ClassInstance _ envs parents) = "Class " ++ show envs ++ ", parents: " ++ show parents
  show (MethodClosure md env) = "Method closure: " ++ show md ++ " in: " ++ show env
  show Null = "Null"
  show (Ref r) = "Ref " ++ show r
  show (ListLit l) = show l

data Context = Context
  { env :: Env,
    store :: Store,
    out :: [String],
    given :: Val,
    failed :: Bool,
    res :: Val,
    seed :: Ref
  }
  deriving (Show, Generic, Eq)

instance NFData Val where
  rnf :: Val -> ()
  rnf (BoolLit b) = rnf b
  rnf (IntLit i) = rnf i
  rnf (Vec ints) = rnf ints
  rnf (ObjInstance r s env vals) = rnf r `seq` rnf s `seq` rnf env `seq` rnf vals
  rnf (ClassInstance _ envs parents) = rnf envs `seq` rnf parents
  rnf (MethodClosure md env) = rnf md `seq` rnf env
  rnf Null = ()
  rnf (Ref r) = rnf r
  rnf (ListLit l) = rnf l

instance NFData Context where
  rnf (Context e s o g f r se) = rnf e `seq` rnf s `seq` rnf o `seq` rnf g `seq` rnf f `seq` rnf r `seq` rnf se

instance ToJSON Context where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Context
instance Binary Context

type JavaState = State Context

bindClasses :: [ClassDecl] -> Context -> Context
bindClasses [] c = c
bindClasses ((ClassDecl id _ _ _) : xs) c = bindClasses xs $ c {env = M.insert id (Ref curr_seed) (env c), seed = new_seed, store = M.insert curr_seed Null (store c)}
  where
    curr_seed = seed c
    new_seed = curr_seed + 1

newAtom :: JavaState Ref
newAtom = do
  ctx <- get
  put ctx {seed = seed ctx + 1}
  return $ seed ctx

initVal :: Type -> Val
initVal TInt = IntLit 0
initVal TBoolean = BoolLit False
initVal TIntArray = Vec []
initVal (TIdentifier _) = Null

declareVariables :: [VarDecl] -> JavaState Env
declareVariables [] = return M.empty
declareVariables (VarDecl t id : xs) =
  declareVariables xs >>= \new_env -> do
    r <- newAtom
    ctx <- get
    put $ ctx {store = M.insert r (initVal t) (store ctx)}
    return $ M.insert id (Ref r) new_env

declareLocalVariables :: [VarDecl] -> JavaState Env
declareLocalVariables [] = return M.empty
declareLocalVariables (VarDecl t id : xs) =
  declareLocalVariables xs >>= \new_env -> do
    ctx <- get
    return $ M.insert id (initVal t) new_env

matchFormals :: [FormalListElem] -> Val -> JavaState Env
matchFormals [] (ListLit []) = return M.empty
matchFormals (FormalListElem t id : xs) (ListLit (l : ls)) =
  matchFormals xs (ListLit ls) >>= \new_env -> do
    r <- newAtom
    modify (\ctx -> ctx {store = M.insert r l (store ctx)})
    return $ M.insert id (Ref r) new_env
matchFormals _ _ = error "Wrong argument list"

execL :: [Statement] -> JavaState ()
execL = foldr ((>>) . exec) (return ())

retrieveFields :: [Val] -> JavaState Env
retrieveFields [] = return M.empty
retrieveFields (ObjInstance ref c vals parent : xs) = do
  penv <- retrieveFields parent
  return $ M.union vals penv
retrieveFields _ = return M.empty

declareMethod :: MethodDecl -> JavaState Env
declareMethod md@(MethodDecl _ ident fl vars statements ret) = do 
  cenv <- gets env 
  return $ M.insert ident (MethodClosure md cenv) M.empty

runMethod :: Val -> Val -> JavaState Val
runMethod (ListLit (ObjInstance ref c vals parents : args)) (MethodClosure (MethodDecl _ _ fl vars statements ret) cenv) = do
  let local_env = M.insert "this" (Ref ref) cenv
  arg_env <- matchFormals fl (ListLit args)
  var_env <- declareVariables vars
  par_env <- retrieveFields parents
  let run_env = foldr M.union var_env [arg_env, vals, par_env, local_env]
  ctx <- get
  ctx_env <- gets env
  put $ ctx {env = run_env}
  execL statements
  res <- eval ret
  ctx' <- get
  put $ ctx' {env = ctx_env}
  return res
runMethod _ _ = error "Method arguments are incorrect."

declareMethods :: [MethodDecl] -> JavaState Env
declareMethods [] = return M.empty
declareMethods (m : ms) = do
  env1 <- declareMethod m
  env2 <- declareMethods ms
  return $ M.union env1 env2

parentToList :: Maybe String -> [String]
parentToList (Just s) = [s]
parentToList Nothing = []

declareClassVal :: ClassDecl -> JavaState Val
declareClassVal cd@(ClassDecl id parent vars methods) = do 
  declareMethods methods >>= \methods_env -> return $ ClassInstance cd methods_env (parentToList parent)

genObject :: ClassDecl -> JavaState Val
genObject (ClassDecl id parent vars methods) = do
  r <- newAtom
  field_map <- declareVariables vars
  pobj <- case parent of
    (Just c) -> do
      ctx <- get
      case M.lookup c (env ctx) of
        (Just (Ref r)) -> do
          pclass <- deref r
          case pclass of 
            (ClassInstance pcd _ _) -> do
              pgened <- genObject pcd
              return $ Just pgened
            _ -> error "Parent wrong representation."
        _ -> error "Parent not found"
    Nothing -> return Nothing
  let obj = case pobj of
        Just uobj -> ObjInstance r id field_map [uobj]
        Nothing -> ObjInstance r id field_map []
  modify (\ctx -> ctx {store = M.insert r obj (store ctx)})
  return obj

classId :: ClassDecl -> String
classId (ClassDecl id _ _ _) = id

declareClass :: ClassDecl -> JavaState ()
declareClass cls = declareClassVal cls >>= \inst -> modify (doUpdate inst)
  where 
    doUpdate inst ctx = 
      case M.lookup (classId cls) (env ctx) of 
        (Just (Ref r)) -> ctx { store = M.insert r inst (store ctx) }

declareClasses :: [ClassDecl] -> JavaState ()
declareClasses = foldr ((>>) . declareClass) (return ())

computeClassMembers :: String -> JavaState Env
computeClassMembers cls_name = do
  ctx <- get
  case M.lookup cls_name (env ctx) of
    (Just (Ref r)) -> do 
      res <- deref r
      case res of 
        ((ClassInstance _ field_map [])) -> return field_map
        ((ClassInstance _ field_map (cls : _))) -> computeClassMembers cls >>= \penv -> return $ M.union field_map penv
    m -> error $ "Class not found.: " ++ cls_name ++ " -> " ++ show ctx

listLitAppend :: Val -> Val -> Val
listLitAppend (ListLit l1) (ListLit l2) = ListLit $ l1 ++ l2
listLitAppend l1 l2 = error $ "INvalid list append[l1=" ++ show l1 ++ ", l2=" ++ show l2 ++ "]"

evalActuals :: [Expr] -> JavaState Val
evalActuals [] = return $ ListLit []
evalActuals (x : xs) = eval x >>= \val -> evalActuals xs >>= \val2 -> return $ listLitAppend (ListLit [val]) val2

lastRef :: Int -> JavaState Int
lastRef r =
  gets store >>= \sto -> case M.lookup r sto of
    Just (Ref r') -> lastRef r'
    Just _ -> return r
    Nothing -> error "Dereference goes to nothing"

deref :: Int -> JavaState Val
deref r = gets store >>= \sto -> lastRef r >>= \r' -> return $ fromJust $ M.lookup r' sto

eval :: Expr -> JavaState Val
eval (Integer i) = return $ IntLit i
eval (Boolean b) = return $ BoolLit b
eval (Identifier i) = do
  ctx <- get
  case M.lookup i (env ctx) of
    (Just (Ref r)) -> deref r
    (Just v) -> return v
    w -> error $ "Id is undefined: " ++ show w ++ show i
eval (LTh ls rs) =
  eval ls >>= \lsv ->
    eval rs >>= \rsv -> case (lsv, rsv) of
      (IntLit ll, IntLit lr) -> return $ BoolLit (ll < lr)
      (l, r) -> get >>= \ctx -> error $ show l ++ "( " ++ show ls ++ ") < " ++ show r ++ "( " ++ show rs ++ ")"
eval (Band ls rs) =
  eval ls >>= \lsv ->
    eval rs >>= \rsv -> case (lsv, rsv) of
      (BoolLit ll, BoolLit lr) -> return $ BoolLit (ll && lr)
      (l, r) -> get >>= \ctx -> error $ show l ++ "( " ++ show ls ++ ") && " ++ show r ++ "( " ++ show rs ++ ")"
eval (Add ls rs) =
  eval ls >>= \lsv ->
    eval rs >>= \rsv -> case (lsv, rsv) of
      (IntLit ll, IntLit lr) -> return $ IntLit (ll + lr)
      _ -> undefined
eval (Mul ls rs) =
  eval ls >>= \lsv ->
    eval rs >>= \rsv -> case (lsv, rsv) of
      (IntLit ll, IntLit lr) -> return $ IntLit (ll * lr)
      _ -> undefined
eval (Sub ls rs) =
  eval ls >>= \lsv ->
    eval rs >>= \rsv -> case (lsv, rsv) of
      (IntLit ll, IntLit lr) -> return $ IntLit (ll - lr)
      _ -> undefined
eval (NewObj ident) =
  gets env >>= \e -> case M.lookup ident e of
    (Just (Ref r)) -> deref r >>= \((ClassInstance cd envs parents)) -> genObject cd
    v -> gets env >>= \ctx -> error $ "Newobj not a class instance: " ++ ident ++ " " ++ show ctx ++ "\n"
eval This =
  gets env >>= \env' -> case M.lookup "this" env' of
    (Just (Ref r)) -> gets store <&> (fromJust . M.lookup r)
    _ -> error "this is not a reference"
eval (MethodCall target name args) =
  eval target >>= \case
    (ObjInstance id cls objenv parent) ->
      computeClassMembers cls >>= \menv ->
        case M.lookup name menv of
          Just md -> evalActuals args >>= \actuals -> runMethod (listLitAppend (ListLit [ObjInstance id cls objenv parent]) actuals) md
          _ -> get >>= \ctx -> error $ "Calling method on something that is not a method: " ++ name ++ " Env: " ++ show menv ++ show cls ++ show parent
    w -> error $ "Not an object: " ++ show w
eval (ArrAccess target loc) =
  eval target >>= \target' ->
    eval loc >>= \loc' -> case target' of
      (Vec l) -> case loc' of
        (IntLit index) -> return $ IntLit $ l !! index
        _ -> error "Location is invalid"
      _ -> error "Not a vector access"
eval (NewArray e) =
  eval e >>= \case
    (IntLit len) -> return $ Vec [0 .. len]
    _ -> error "Nope"
eval (Length e) =
  eval e >>= \case
    (Vec l) -> return $ IntLit $ length l
    _ -> error "INvalid length"
eval (Not e) =
  eval e >>= \case
    (BoolLit l) -> return $ BoolLit $ not l
    _ -> error "INvalid not"

exec :: Statement -> JavaState ()
exec (While e s) =
  eval e >>= \case
    BoolLit True -> exec s >> exec (While e s)
    BoolLit False -> return ()
    _ -> undefined
exec (Block statements) = foldr ((>>) . exec) (return ()) statements
exec (Print e) = eval e >>= \res -> modify (\ctx -> ctx {out = out ctx ++ [show res ++ "\n"]})
exec (If e st sf) =
  eval e >>= \case
    BoolLit True -> exec st
    BoolLit False -> exec sf
    v -> error $ "If clause not a bool." ++ show (If e st sf) ++ "resut: " ++ show v
exec (Assign ident e) =
  eval e >>= \res -> do
    ctx <- get
    case M.lookup ident (env ctx) of
      Just (Ref r) -> case res of
        (ObjInstance ref _ _ _) -> modify (\ctx -> ctx {store = M.insert r (Ref ref) (store ctx)})
        val -> modify (\ctx -> ctx {store = M.insert r val (store ctx)})
      Just val -> case res of
        (ObjInstance ref _ _ _) -> modify (\ctx -> ctx {env = M.insert ident (Ref ref) (env ctx)})
        val -> modify (\ctx -> ctx {env = M.insert ident val (env ctx)})
      _ -> error "Invalid identifier"
exec (ArrAssign ident loc e) =
  eval loc >>= \loc' ->
    eval e >>= \e' -> do
      ctx <- get
      case M.lookup ident (env ctx) of
        Just (Ref r) -> deref r >>= \l -> modify (\ctx -> ctx {store = M.insert r (updateVec l loc' e') (store ctx)})
        Just l -> modify (\ctx -> ctx {env = M.insert ident (updateVec l loc' e') (env ctx)})
        Nothing -> error "INvalid array assign"

updateListAtIndex :: Int -> a -> [a] -> [a]
updateListAtIndex 0 val (x : xs) = val : xs
updateListAtIndex n val (x : xs) = x : updateListAtIndex (n - 1) val xs
updateListAtIndex _ _ _ = error "Invalid list update"

updateVec :: Val -> Val -> Val -> Val
updateVec (Vec l) (IntLit loc) (IntLit newval) = Vec (updateListAtIndex loc newval l)
updateVec _ _ _ = error "Invalid vector update"


boundNames :: [ClassDecl] -> [String] 
boundNames = map classId

forwardLink :: [String] -> JavaState ()
forwardLink [] = return ()
forwardLink (n:ns) = do 
  r <- newAtom
  modify $ \ctx -> ctx { env = M.insert n (Ref r) (env ctx), store = M.insert r Null (store ctx) }
  forwardLink ns

runMain (MainClass _ _ s) = exec s

runProgram (Program m cc) = execState runProgram' initialContext
  where
    runProgram' = forwardLink names >> declareClasses cc >> runMain m
    names = boundNames cc

initialContext = Context {env = M.empty, store = M.empty, out = [], given = Null, failed = False, res = Null, seed = 1}
