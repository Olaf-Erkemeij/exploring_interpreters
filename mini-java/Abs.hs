{-# LANGUAGE DeriveGeneric #-}

module Abs where

import Control.DeepSeq (NFData, rnf)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, defaultOptions, genericToEncoding, toEncoding)
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
instance ToJSON Expr where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Expr
instance Binary Expr

instance NFData Statement
instance ToJSON Statement where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Statement
instance Binary Statement

instance NFData Phrase
instance ToJSON Phrase where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Phrase
instance Binary Phrase

instance NFData Type
instance ToJSON Type where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Type
instance Binary Type

instance NFData FormalListElem
instance ToJSON FormalListElem where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON FormalListElem
instance Binary FormalListElem

instance NFData VarDecl
instance ToJSON VarDecl where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VarDecl
instance Binary VarDecl

instance NFData MethodDecl
instance ToJSON MethodDecl where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON MethodDecl
instance Binary MethodDecl

instance NFData ClassDecl
instance ToJSON ClassDecl where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ClassDecl
instance Binary ClassDecl
 
instance NFData MainClass
instance ToJSON MainClass where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON MainClass
instance Binary MainClass

instance NFData Program
instance ToJSON Program where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Program
instance Binary Program
