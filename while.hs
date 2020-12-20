module Whilelang where

import qualified Data.Map as Map
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

data Literal = LitBool Bool | LitInt Integer deriving (Eq)
instance Show Literal where
    show (LitBool b) = show b
    show (LitInt i) = show i

data Expr = Leq Expr Expr | Plus Expr Expr | LitExpr Literal | Id String
instance Show Expr where
    show (Leq e1 e2) = (show e1) ++ "<=" ++ (show e2)
    show (Plus e1 e2) = (show e1) ++ "+" ++ (show e2)
    show (LitExpr lit) = show lit
    show (Id s) = show s

data Command = Seq Command Command | Assign String Expr | Print Expr | While Expr Expr Command | Done
instance Show Command where
    show (Print e1) = "Print(" ++ (show e1) ++ ")"
    show (Done) = "Done"
    show (Assign s e) = (show s) ++ " = " ++ (show e)
    show (Seq c1 c2) = "Seq(" ++ (show c1) ++ ", " ++ (show c2) ++ ")"
    show (While e1 e2 c) = "While(" ++ (show e2) ++ "){ " ++ (show c) ++ " }"

type Store = State (Map.Map String Literal)

evalPlus :: Expr -> Expr -> Store Expr
evalPlus (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitInt (l1 + l2)
evalPlus (LitExpr l1) l2 = do
    l2' <- evalExpr l2
    return (Plus (LitExpr l1) l2')
evalPlus l1 l2 = do
    l1' <- evalExpr l1
    return (Plus l1' l2)

evalLeq :: Expr -> Expr -> Store Expr
evalLeq (LitExpr (LitInt l1)) (LitExpr (LitInt l2)) = return $ LitExpr $ LitBool (l1 <= l2)
evalLeq (LitExpr l1) e2 = do
    e2' <- evalExpr e2
    return (Leq (LitExpr l1) e2')
evalLeq e1 e2 = do
    e1' <- evalExpr e1
    return (Leq e1' e2)

evalExpr :: Expr -> Store Expr
evalExpr (LitExpr e) = return $ LitExpr e
evalExpr (Plus e1 e2) = evalPlus e1 e2
evalExpr (Leq e1 e2) = evalLeq e1 e2
evalExpr (Id s) = do
    m <- get
    let l = Map.lookup s m
    case l of
        Just lit -> return $ LitExpr lit
        Nothing -> error $ "Invalid Id: " ++ s

evalExpr' :: Expr -> Store Expr
evalExpr' (LitExpr e) = return $ LitExpr e
evalExpr' e = do
    e' <- evalExpr e
    evalExpr' e'

store :: String -> Expr -> Store Command
store s (LitExpr l) = do
    lut <- get
    put $ Map.insert s l lut
    return Done

evalCommand :: Command -> WriterT [String] Store Command
evalCommand (Print e) = do
    x <- (lift . evalExpr') e
    tell $ [(show x)]
    return Done
evalCommand (Assign id e) = do
    lit <- (lift . evalExpr') e
    c <- lift $ store id lit
    return c
evalCommand (Seq Done c2) = return c2
evalCommand (Seq c1 c2) = do
    c1' <- evalCommand c1
    return $ Seq c1' c2
evalCommand (While (LitExpr (LitBool False)) e2 c) = return Done
evalCommand (While (LitExpr (LitBool True)) e2 c) = return $ Seq c (While e2 e2 c)
evalCommand (While e1 e2 c) = do
    e1' <- (lift . evalExpr') e1
    return $ While e1' e2 c


evalCommand' :: Command -> WriterT [String] Store Command
evalCommand' Done = return Done
evalCommand' c = do
    c' <- evalCommand c
    evalCommand' c'


-- Below are some helpers to create a Command and fully evaluate it.
-- Example:
-- ghci> let x = wprint (intToExpr 10) `wseq` (wprint (intToExpr 100) `wseq` wprint (intToExpr 200))
-- ghci> runCommand' x
-- ["10","100","200"]
-- ghci> 
runCommand :: Command -> IO()
runCommand c = do
    let ((_, output), _) = runState (runWriterT (evalCommand' c)) Map.empty
    print output


intToExpr :: Integer -> Expr
intToExpr = (LitExpr . LitInt)

boolToExpr :: Bool -> Expr
boolToExpr = (LitExpr . LitBool)

while ::  Expr -> Command -> Command
while e c = While e e c

leq :: Expr -> Expr -> Expr
leq e1 e2 = Leq e1 e2

wprint :: Expr -> Command
wprint c = Print c

plus :: Expr -> Expr -> Expr
plus e1 e2 = Plus e1 e2

assign :: String -> Expr -> Command
assign s e = Assign s e

wseq :: Command -> Command -> Command
wseq c1 c2 = Seq c1 c2