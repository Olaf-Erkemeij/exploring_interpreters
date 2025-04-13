{-# LANGUAGE DeriveGeneric #-}
module Abs where

import Test.QuickCheck
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Binary (Binary)

data Expr
  = Integer Int
  | Boolean Bool
  | String String
  | Symbol String
  | Lambda [String] Expr
  | List [Expr]
  deriving (Show, Eq, Generic)

instance NFData Expr
instance ToJSON Expr
instance FromJSON Expr
instance Binary Expr

newtype Program = Program [Expr]

genExprValid :: Gen Expr
genExprValid = genExpr []

genExpr :: [String] -> Gen Expr
genExpr env = sized $ \n ->
  if n <= 0 then genExprTerminal env
  else oneof [ genExprTerminal env
             , genDefine env
             , genIf env (n `div` 2)
             , genLambda env (n `div` 2)
             , genList env (n `div` 2)
             ]

genExprTerminal :: [String] -> Gen Expr
genExprTerminal env = oneof
  [ int
  , if null env then int else Symbol <$> elements env
  ]
  where int = Integer <$> arbitrary `suchThat` (/= 0)

genDefine :: [String] -> Gen Expr
genDefine env = do
  var <- genVarName env
  expr <- genExpr env
  return $ List [Symbol "define", Symbol var, expr]

genIf :: [String] -> Int -> Gen Expr
genIf env n = do
  cond <- elements [Boolean True, Boolean False]
  thenBranch <- resize n (genExpr env)
  elseBranch <- resize n (genExpr env)
  return $ List [Symbol "if", cond, thenBranch, elseBranch]

genLambda :: [String] -> Int -> Gen Expr
genLambda env n = do
  nParams <- choose (1, 3)
  params <- vectorOf nParams (genVarName env)
  body <- resize n (genExpr (env ++ params))
  return $ Lambda params body

genMonop :: Gen Expr
genMonop = Symbol <$> elements ["display"]

genBinop :: Gen Expr
genBinop = Symbol <$> elements ["+", "*", "-", "/"]

genList :: [String] -> Int -> Gen Expr
genList env _ = do
  nExprs <- choose (1, 2)
  exprs <- vectorOf nExprs (genExprTerminal env)
  op <- case nExprs of
    1 -> genMonop
    _ -> genBinop
  return $ List (op : exprs)

genVarName :: [String] -> Gen String
genVarName env = do
  c <- elements ['a'..'z']
  n <- choose (0, 100 :: Int)
  let var = c : show n
  if var `elem` env then genVarName env else return var
