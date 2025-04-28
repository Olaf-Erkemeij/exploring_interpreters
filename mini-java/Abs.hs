{-# LANGUAGE DeriveGeneric #-}

module Abs where

import Control.DeepSeq (NFData, rnf)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Binary (Binary)

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
instance ToJSON Expr
instance FromJSON Expr
instance Binary Expr

instance NFData Statement
instance ToJSON Statement
instance FromJSON Statement
instance Binary Statement

instance NFData Phrase
instance ToJSON Phrase
instance FromJSON Phrase
instance Binary Phrase

instance NFData Type
instance ToJSON Type
instance FromJSON Type
instance Binary Type

instance NFData FormalListElem
instance ToJSON FormalListElem
instance FromJSON FormalListElem
instance Binary FormalListElem

instance NFData VarDecl
instance ToJSON VarDecl
instance FromJSON VarDecl
instance Binary VarDecl

instance NFData MethodDecl
instance ToJSON MethodDecl
instance FromJSON MethodDecl
instance Binary MethodDecl

instance NFData ClassDecl
instance ToJSON ClassDecl
instance FromJSON ClassDecl
instance Binary ClassDecl
 
instance NFData MainClass
instance ToJSON MainClass
instance FromJSON MainClass
instance Binary MainClass

instance NFData Program
instance ToJSON Program
instance FromJSON Program
instance Binary Program
