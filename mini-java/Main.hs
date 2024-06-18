{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Main where
import GLL.Combinators
import Text.Read.Lex (Lexeme(Ident))
import Abs
import Interpreter
import qualified SeqInterpreter as Pi
import System.IO
import Abs (Program, Phrase (PStmt, PMethod))
import Language.Explorer.Tools.REPL
import Language.Explorer.Tools.Protocol
import Language.Explorer.Monadic as EM
import Data.Tree (drawTree)
import JSON
import Test.QuickCheck hiding (within)
import Control.DeepSeq
import Control.Monad
import GHC.DataSize


instance Arbitrary Expr where
    arbitrary = sized expr
        where
            expr _ = oneof [Integer <$> arbitrary, Boolean <$> arbitrary, Identifier <$> oneof (map return [[x] | x <- ['a'..'t']])]

instance Arbitrary Statement where
    arbitrary = sized stmt
        where
            stmt 0 = oneof [assgn, prnt]
            stmt _ = oneof [
                Block <$> listOf (stmt 0),
                If <$> (Boolean <$> arbitrary) <*> stmt 0 <*> stmt 0,
                prnt,
                assgn]
            assgn = oneof
                [ Assign <$> oneof (map return [[x] | x <- ['a'..'j']]) <*> (Integer <$> arbitrary)
                , Assign <$> oneof (map return [[x] | x <- ['k'..'t']]) <*> (Boolean <$> arbitrary)
                ]
            prnt = Print <$> arbitrary

instance Arbitrary Type where
    arbitrary = oneof [return TInt, return TBoolean]

instance Arbitrary Phrase where
    arbitrary = oneof [PExpr <$> arbitrary, PStmt <$> arbitrary]

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
           <||>  parens parseExpr

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

calcSize :: Handle -> Int -> Float -> IO ()
calcSize handle n p = do
    explorer <- randomTree n p
    size <- recursiveSize (EM.config explorer)
    hPutStrLn handle $ show n ++ "," ++ show p ++ "," ++ show size

runExperiment1 :: Handle -> Int -> IO ()
runExperiment1 handle 0 = return ()
runExperiment1 handle n = do
    runExperiment1 handle (n-1)
    calcSize handle n 0.5

runExperiment2 :: Handle -> Int -> IO ()
runExperiment2 handle 0 = return ()
runExperiment2 handle n = do
    runExperiment2 handle (n `div` 10)
    calcSize handle n 0.5

main :: IO ()
main = do
    handle <- openFile "../results/data/data1.csv" WriteMode
    hPutStrLn handle "N,P,Size"
    runExperiment1 handle 1000
    hClose handle

    handle2 <- openFile "../results/data/data2.csv" WriteMode
    hPutStrLn handle2 "N,P,Size"
    runExperiment2 handle2 (10^6)
    hClose handle2


-- While rpel
-- repl = R.repl (const "While> ") parser ":" R.metaTable  (\_ ex -> return ex) (\_ -> return ()) (EM.mkExplorerNoSharing definterpM initialConfig)

-- replParser :: String -> Context -> Either Phrase
replParser s _ = parser s

-- Java repl
javaREPL = repl (const "Java> ") replParser ":" metaTable (\_ ex -> return ex) (\o -> putStr (concat o)) explorer

instance ExplorerPostValue Phrase Context [String]
explorer  = mkExplorerNoSharing Pi.runPhrase initialContext

fromFile :: String -> IO ()
fromFile file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    case parse parsePhrase (lexer lexerSettings contents) of
        [] -> return ()
        (x:_) -> do
            (Just ctx, out) <- Pi.runPhrase x initialContext
            print $ env ctx
            print $ store ctx
            putStrLn (concat $ out)

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
        \num_aux = num * (this.computeFac(num-2)) ;\
    \return num_aux ;}}"

runCommand :: Phrase -> Context -> IO Context
runCommand p c = do
    (Just ctx, out) <- Pi.runPhrase p c
    putStr (concat $ out)
    return ctx

initialVars :: Phrase
initialVars = foldr1 PSeq $ [PVar (VarDecl TInt [x]) | x <- ['a'..'j']] ++ [PVar (VarDecl TBoolean [x]) | x <- ['k'..'t']]

initialiseContext :: IO Context
-- initialiseContext = foldl (>>=) (return initialContext) (map runCommand initialVars)
initialiseContext = runCommand initialVars initialContext

runRandomPhrases :: IO ()
runRandomPhrases = do
    initContext <- initialiseContext
    x <- generate arbitrary :: IO [Phrase]
    ctx <- foldl (>>=) (return initContext) (map runCommand x)
    print $ env ctx
    print $ store ctx

printExplorer :: Explorer Phrase IO Context [String] -> IO ()
printExplorer ex = do
    let tree = toTree ex
    putStrLn . drawTree $ fmap (show . fst) tree

randomTree :: Int -> Float -> IO (Explorer Phrase IO Context [String])
randomTree 1 _ = mkExplorerNoSharing Pi.runPhrase <$> initialiseContext
randomTree n p = do
    explr <- randomTree (n-1) p
    randPhrase <- generate (arbitrary :: Gen Phrase)
    jumpCond <- generate $ choose (0.0, 1.0)
    explr' <- if jumpCond <= p
              then do
                jumpRef <- generate (choose (1, EM.currRef explr - 1))
                case EM.jump jumpRef explr of
                  Just newExplr -> return newExplr
                  Nothing -> return explr
              else return explr
    (newExplr, _) <- EM.execute randPhrase explr'
    return newExplr

measureBytes :: Context -> IO ()
measureBytes ex = {-# SCC "config_size" #-} ex `deepseq` return ()