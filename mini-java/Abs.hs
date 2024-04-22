module Abs where


data Expr = Integer Int
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
          deriving (Show, Eq)

data Statement = Block [Statement]
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Assign String Expr
               | ArrAssign String Expr Expr
               deriving (Show, Eq)

data Phrase = PExpr Expr 
            | PStmt Statement
            | PVar VarDecl 
            | PClass ClassDecl
            | PMethod MethodDecl
            | PMethodCall String [Expr]
            | PSeq Phrase Phrase 
            deriving (Show, Eq)

data Type = TInt | TBoolean | TIntArray | TIdentifier String deriving (Show, Eq)
data FormalListElem = FormalListElem Type String deriving (Show, Eq)
data VarDecl = VarDecl Type String deriving (Show, Eq)
data MethodDecl = MethodDecl Type String [FormalListElem] [VarDecl] [Statement] Expr deriving (Show, Eq)
data ClassDecl = ClassDecl String (Maybe String) [VarDecl] [MethodDecl] deriving (Show, Eq)
data MainClass = MainClass String String Statement deriving (Show, Eq)
data Program = Program MainClass [ClassDecl] deriving (Show, Eq)

