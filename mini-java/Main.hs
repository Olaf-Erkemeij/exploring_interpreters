{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where
import GLL.Combinators 
import Text.Read.Lex (Lexeme(Ident))
import Abs
import Interpreter
import qualified SeqInterpreter as Pi
import System.IO
import Abs (Program, Phrase (PStmt, PMethod))
import Language.Explorer.Tools.Protocol
import Language.Explorer.Monadic
import JSON

type Parser a = BNF Token a
skipKeyword const l _ r = const l r

exprOpTable = opTableFromList
    [ (0.5, [("&&", Infix (skipKeyword Band) LAssoc) ])
    , (0.7, [("<", Infix (skipKeyword LTh) RAssoc )])
    , (1.0, [("+", Infix (skipKeyword Add) LAssoc), ("-", Infix (skipKeyword Sub) LAssoc ) ])
    , (2.0, [("*", Infix (skipKeyword Mul) LAssoc) ])
    ]

parseBool :: Parser Bool
parseBool = 
    "Bool" <::=> True <$$ keyword "true"
           <||> False <$$ keyword "false"

parseExpr :: Parser Expr
parseExpr = 
    "Expr" <::=> Integer <$$> int_lit
           <||>  Boolean <$$> parseBool
           <||>  Identifier <$$> id_lit
           <||>  This <$$ keyword "this"
           <||>  NewObj <$$> (keyword "new" **> alt_id_lit <** keychar '(' <** keychar ')')
           <||>  NewArray <$$> (keyword "new" **> keyword "int" **>  within (keychar '[') parseExpr (keychar ']'))
           <||>  Not <$$> (keychar '!' **> parseExpr)
           <||>  MethodCall <$$> parseExpr <** keychar '.' <**> alt_id_lit <**> parens parseExprList
           <||>  ArrAccess <$$> parseExpr <**> within (keychar '[') parseExpr (keychar ']')
           <||>  fromOpTable "+" exprOpTable parseExpr
           <||>  Length <$$> parseExpr <** (keychar '.' **> keyword "length")
           <||> parens parseExpr

parseExprList :: Parser [Expr]
parseExprList =
    "Expr list" <::=> multipleSepBy parseExpr (keychar ',')

parseStatement :: Parser Statement
parseStatement = 
    "Statement" <::=> Block <$$> (keychar '{' **> (many parseStatement <** keychar '}'))
                <||>  If <$$> (keyword "if" **> parens parseExpr) <**> parseStatement <** keyword "else" <**> parseStatement
                <||>  Print <$$> (keyword "System" **> keychar '.' **> keyword "out" **> keychar '.' **> keyword "println" **> parens parseExpr <** keychar ';')
                <||> Assign <$$> id_lit <** keychar '=' <**> parseExpr <** keychar ';'
                <||> While <$$> (keyword "while" **> parens parseExpr) <**> parseStatement
                <||> ArrAssign <$$> (id_lit <** keychar '[') <**> (parseExpr <** keychar ']') <**> (keychar '=' **> parseExpr <** keychar ';') 
parseType :: Parser Type
parseType =
    "Type" <::=> TInt <$$ keyword "int"
           <||>  TBoolean <$$ keyword "boolean"
           <||>  TIdentifier <$$> alt_id_lit
           <||>  TIntArray <$$ (keyword "int" **> keychar '[' **> keychar ']')

parseFormalListElem :: Parser [FormalListElem]
parseFormalListElem =
    "FormalListElem" <::=> multipleSepBy (FormalListElem <$$> parseType <**> id_lit) (keychar ',')

parseVarDecl :: Parser VarDecl
parseVarDecl = 
    "VarDecl" <::=> VarDecl <$$> parseType <**> id_lit <** keychar ';'

parseMethodDecl :: Parser MethodDecl
parseMethodDecl =
    "MethodDecl" <::=> MethodDecl <$$> (keyword "public" **> parseType) <**> alt_id_lit <**> parens parseFormalListElem
        <**> (keychar '{' **> many parseVarDecl) <**> many parseStatement <**> (keyword "return" **> parseExpr <** keychar ';' <** keychar '}')

parseClassDecl :: Parser ClassDecl
parseClassDecl = 
    "ClassDecl" <::=> ClassDecl <$$> (keyword "class" **> alt_id_lit) <**> optional (keyword "extends" **> alt_id_lit) <**>
        (keychar '{' **> many parseVarDecl) <**> (many parseMethodDecl <** keychar '}')

parseMainClass :: Parser MainClass 
parseMainClass =
    "MainClass" <::=> MainClass <$$> (keyword "class" **> alt_id_lit) <**> (keychar '{' **> keyword "public" **> keyword "static" **> keyword "void" **> keyword "main" **> keychar '(' 
        **> keyword "String" **> keychar '[' **> keychar ']' **> id_lit <** keychar ')') <**> parseStatement <** keychar '}'

parseProgram :: Parser Program
parseProgram = 
    "Program" <::=> Program <$$> parseMainClass <**> many parseClassDecl

parsePhrase :: Parser Phrase
parsePhrase = 
    "Phrase" <::=> PExpr <$$> parseExpr <** keychar ';'
             <||>  PStmt <$$> parseStatement 
             <||>  PClass <$$> parseClassDecl 
             <||>  PMethod <$$> parseMethodDecl
             <||>  PVar <$$> parseVarDecl
             <||>  PMethodCall <$$> alt_id_lit <**> parens parseExprList
             <||>  PSeq <$$> parsePhrase <<<**> parsePhrase

miniJavaKeywords = 
    [ "String"
    , "while"
    , "System"
    , "boolean"
    , "class"
    , "else"
    , "extends"
    , "true"
    , "false"
    , "if"
    , "int"
    , "length"
    , "main"
    , "new"
    , "out"
    , "println"
    , "public"
    , "return"
    , "static"
    , "this"
    , "void"
    , "&&"
    , "+"
    , "-"
    , "*"
    , "<"]

lexerSettings :: LexerSettings
lexerSettings = emptyLanguage
  { keywords = miniJavaKeywords
  , keychars = "()[]{},.;!="
  }


parser s = case lexerEither lexerSettings s of
    Left err -> Left err
    Right tok -> case parseWithOptionsAndError [maximumErrors 1]  parsePhrase tok of
        Left err -> Left err 
        Right (x:_) -> Right x

main :: IO ()
main = run (\v _ -> return v)

run = serve "5002" explorer parser

instance ExplorerPostValue Phrase Context [String]
explorer  = mkExplorerNoSharing Pi.runPhrase initialContext 

fromFile :: String -> IO ()
fromFile file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    case parse parseProgram (lexer lexerSettings contents) of 
        [] -> return ()
        (x:_) -> do 
            let ctx = runProgram x 
            print $ env ctx
            print $ store ctx
            putStrLn (concat $ out ctx)


factorial :: String 
factorial = "class Factorial{\
    \public static void main(String[] a){\
	\System.out.println(new Fac().computeFac(10));    }}\
\class Fac {\
    \public int computeFac(int num){ \
	\int num_aux ;\
	\if (num < 1) \
        \num_aux = 1 ;\
    \else \
        \num_aux = num * (this.computeFac(num-1)) ;\
    \return num_aux ;}}"