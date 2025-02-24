{-# LANGUAGE DeriveGeneric #-}

module Abs where

import Control.DeepSeq (NFData, rnf)
import GHC.Generics (Generic)

data Expr
  = Integer Int
  | Boolean Bool
  | Identifier String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Band Expr Expr
  | LTh Expr Expr
  | This
  | Length Expr
  | Not Expr
  | NewArray Expr
  | NewObj String
  | MethodCall Expr String [Expr]
  | ArrAccess Expr Expr
  deriving (Show, Eq, Generic)

data Statement
  = Block [Statement]
  | If Expr Statement Statement
  | While Expr Statement
  | Print Expr
  | Assign String Expr
  | ArrAssign String Expr Expr
  deriving (Show, Eq, Generic)

data Phrase
  = PExpr Expr
  | PStmt Statement
  | PVar VarDecl
  | PClass ClassDecl
  | PMethod MethodDecl
  | PMethodCall String [Expr]
  | PSeq Phrase Phrase
  deriving (Show, Eq, Generic)

data Type
  = TInt
  | TBoolean
  | TIntArray
  | TIdentifier String
  deriving (Show, Eq, Generic)

data FormalListElem = FormalListElem Type String
  deriving (Show, Eq, Generic)

data VarDecl = VarDecl Type String
  deriving (Show, Eq, Generic)

data MethodDecl = MethodDecl Type String [FormalListElem] [VarDecl] [Statement] Expr
  deriving (Show, Eq, Generic)

data ClassDecl = ClassDecl String (Maybe String) [VarDecl] [MethodDecl]
  deriving (Show, Eq, Generic)

data MainClass = MainClass String String Statement
  deriving (Show, Eq, Generic)

data Program = Program MainClass [ClassDecl]
  deriving (Show, Eq, Generic)

instance NFData Expr

instance NFData Statement

instance NFData Phrase

instance NFData Type

instance NFData FormalListElem

instance NFData VarDecl

instance NFData MethodDecl

instance NFData ClassDecl

instance NFData MainClass

instance NFData Program
