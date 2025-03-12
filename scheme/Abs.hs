module Abs where

data Expr
  = Integer Int
  | Boolean Bool
  | String String
  | Symbol String
  | Lambda [String] Expr
  | List [Expr]
  deriving (Show, Eq)

newtype Program = Program [Expr]
