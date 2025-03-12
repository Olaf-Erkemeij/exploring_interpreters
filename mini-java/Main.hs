{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Abs
import Control.DeepSeq
import Control.Monad
import Data.Tree (drawTree)
import GHC.DataSize
import GLL.Combinators
import Interpreter
import JSON
import Language.Explorer.Monadic as EM
import Language.Explorer.Tools.Protocol
import Language.Explorer.Tools.REPL
import qualified SeqInterpreter as Pi
import System.IO
import Test.QuickCheck hiding (within)
import Text.Read.Lex (Lexeme (Ident))
import Data.Functor

genIntId :: Gen String
genIntId = (:[]) <$> choose ('a', 'j')

genBoolId :: Gen String
genBoolId = (:[]) <$> choose ('k', 't')

-- Generate an expression that reduces to a boolean
genBoolExpr :: Int -> Gen Expr
genBoolExpr 0 = oneof
  [ Boolean <$> arbitrary
  , Identifier <$> genBoolId
  ]
genBoolExpr n = oneof
  [ Boolean <$> arbitrary
  , Identifier <$> genBoolId
  , Band <$> sub n <*> sub n
  , LTh <$> subInt n <*> subInt n
  ]
  where
    sub = genBoolExpr . (`div` 2)
    subInt = genIntExpr . (`div` 2)

-- Generate an expression that reduces to an integer
genIntExpr :: Int -> Gen Expr
genIntExpr 0 = oneof
  [ Integer <$> arbitrary
  , Identifier <$> genIntId
  ]
genIntExpr n = oneof
  [ Integer <$> arbitrary
  , Identifier <$> genIntId
  , Add <$> sub n <*> sub n
  , Mul <$> sub n <*> sub n
  , Sub <$> sub n <*> sub n
  ]
  where
    sub = genIntExpr . (`div` 2)

instance Arbitrary Expr where
  arbitrary = sized $ \n -> oneof [genBoolExpr n, genIntExpr n]

instance Arbitrary Statement where
  arbitrary = sized stmt
    where
      stmt 0 = oneof [assgn, prnt]
      stmt n =
        oneof
          [ Block <$> listOf (stmt 0)
          , If <$> genBoolExpr (n `div` 2) <*> stmt (n `div` 2) <*> stmt (n `div` 2)
          , prnt
          , assgn
          ]
      assgn =
        oneof
          [ Assign <$> genIntId <*> (Integer <$> arbitrary)
          , Assign <$> genBoolId <*> (Boolean <$> arbitrary)
          ]
      prnt = Print <$> arbitrary

instance Arbitrary Phrase where
  arbitrary = oneof [PExpr <$> arbitrary, PStmt <$> arbitrary]

type Parser a = BNF Token a

skipKeyword const l _ = const l

exprOpTable =
  opTableFromList
    [ (0.5, [("&&", Infix (skipKeyword Band) LAssoc)]),
      (0.7, [("<", Infix (skipKeyword LTh) RAssoc)]),
      (1.0, [("+", Infix (skipKeyword Add) LAssoc), ("-", Infix (skipKeyword Sub) LAssoc)]),
      (2.0, [("*", Infix (skipKeyword Mul) LAssoc)])
    ]

parseBool :: Parser Bool
parseBool =
  "Bool"
    <::=> True <$$ keyword "true"
      <||> False <$$ keyword "false"

parseExpr :: Parser Expr
parseExpr =
  "Expr"
    <::=> Integer <$$> int_lit
      <||> Boolean <$$> parseBool
      <||> Identifier <$$> id_lit
      <||> This <$$ keyword "this"
      <||> NewObj <$$> (keyword "new" **> alt_id_lit <** keychar '(' <** keychar ')')
      <||> NewArray <$$> (keyword "new" **> keyword "int" **> within (keychar '[') parseExpr (keychar ']'))
      <||> Not <$$> (keychar '!' **> parseExpr)
      <||> MethodCall <$$> parseExpr <** keychar '.' <**> alt_id_lit <**> parens parseExprList
      <||> ArrAccess <$$> parseExpr <**> within (keychar '[') parseExpr (keychar ']')
      <||> fromOpTable "+" exprOpTable parseExpr
      <||> Length <$$> parseExpr <** (keychar '.' **> keyword "length")
      <||> parens parseExpr

parseExprList :: Parser [Expr]
parseExprList =
  "Expr list" <::=> multipleSepBy parseExpr (keychar ',')

parseStatement :: Parser Statement
parseStatement =
  "Statement"
    <::=> Block <$$> (keychar '{' **> (many parseStatement <** keychar '}'))
      <||> If <$$> (keyword "if" **> parens parseExpr) <**> parseStatement <** keyword "else" <**> parseStatement
      <||> Print <$$> (keyword "System" **> keychar '.' **> keyword "out" **> keychar '.' **> keyword "println" **> parens parseExpr <** keychar ';')
      <||> Assign <$$> id_lit <** keychar '=' <**> parseExpr <** keychar ';'
      <||> While <$$> (keyword "while" **> parens parseExpr) <**> parseStatement
      <||> ArrAssign <$$> (id_lit <** keychar '[') <**> (parseExpr <** keychar ']') <**> (keychar '=' **> parseExpr <** keychar ';')

parseType :: Parser Type
parseType =
  "Type"
    <::=> TInt <$$ keyword "int"
      <||> TBoolean <$$ keyword "boolean"
      <||> TIdentifier <$$> alt_id_lit
      <||> TIntArray <$$ (keyword "int" **> keychar '[' **> keychar ']')

parseFormalListElem :: Parser [FormalListElem]
parseFormalListElem =
  "FormalListElem" <::=> multipleSepBy (FormalListElem <$$> parseType <**> id_lit) (keychar ',')

parseVarDecl :: Parser VarDecl
parseVarDecl =
  "VarDecl" <::=> VarDecl <$$> parseType <**> id_lit <** keychar ';'

parseMethodDecl :: Parser MethodDecl
parseMethodDecl =
  "MethodDecl"
    <::=> MethodDecl
      <$$> (keyword "public" **> parseType)
      <**> alt_id_lit
      <**> parens parseFormalListElem
      <**> (keychar '{' **> many parseVarDecl)
      <**> many parseStatement
      <**> (keyword "return" **> parseExpr <** keychar ';' <** keychar '}')

parseClassDecl :: Parser ClassDecl
parseClassDecl =
  "ClassDecl"
    <::=> ClassDecl
      <$$> (keyword "class" **> alt_id_lit)
      <**> optional (keyword "extends" **> alt_id_lit)
      <**> (keychar '{' **> many parseVarDecl)
      <**> (many parseMethodDecl <** keychar '}')

parseMainClass :: Parser MainClass
parseMainClass =
  "MainClass"
    <::=> MainClass
      <$$> (keyword "class" **> alt_id_lit)
      <**> ( keychar '{'
               **> keyword "public"
               **> keyword "static"
               **> keyword "void"
               **> keyword "main"
               **> keychar '('
               **> keyword "String"
               **> keychar '['
               **> keychar ']'
               **> id_lit
               <** keychar ')'
           )
      <**> parseStatement
      <** keychar '}'

parseProgram :: Parser Program
parseProgram =
  "Program" <::=> Program <$$> parseMainClass <**> many parseClassDecl

parsePhrase :: Parser Phrase
parsePhrase =
  "Phrase"
    <::=> PExpr <$$> parseExpr <** keychar ';'
      <||> PStmt <$$> parseStatement
      <||> PClass <$$> parseClassDecl
      <||> PMethod <$$> parseMethodDecl
      <||> PVar <$$> parseVarDecl
      <||> PMethodCall <$$> alt_id_lit <**> parens parseExprList
      <||> PSeq <$$> parsePhrase <<<**> parsePhrase

miniJavaKeywords =
  [ "String",
    "while",
    "System",
    "boolean",
    "class",
    "else",
    "extends",
    "true",
    "false",
    "if",
    "int",
    "length",
    "main",
    "new",
    "out",
    "println",
    "public",
    "return",
    "static",
    "this",
    "void",
    "&&",
    "+",
    "-",
    "*",
    "<"
  ]

lexerSettings :: LexerSettings
lexerSettings =
  emptyLanguage
    { keywords = miniJavaKeywords,
      keychars = "()[]{},.;!="
    }

parser :: String -> Either String Phrase
parser s = case lexerEither lexerSettings s of
  Left err -> Left err
  Right tok -> case parseWithOptionsAndError [maximumErrors 1] parsePhrase tok of
    Left err -> Left err
    Right [] -> Left "No parse"
    Right (x : _) -> Right x

calcSize :: Handle -> Explorer Phrase IO Context [String] -> Int -> Float -> Int -> IO ()
calcSize handle explorer n p x = do
  print (n, p, x)
  size1 <- recursiveSizeNF (EM.cmap explorer)
  size2 <- recursiveSizeNF (EM.execEnv explorer)
  hPutStrLn handle $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size1 ++ "," ++ show size2

runExperiment1 :: Handle -> Int -> Float -> IO ()
runExperiment1 handle 0 _ = return ()
runExperiment1 handle n p = do
  runExperiment1 handle (n - 1) p
  explorer <- randomTree n p
  forM_ [1..2] $ \x -> calcSize handle explorer n p x

runExperiment2 :: Handle -> Int -> Float -> IO ()
runExperiment2 handle 0 _ = return ()
runExperiment2 handle n p = do
  runExperiment2 handle (n - 100) p
  explorer <- randomTree n p
  forM_ [1..5] $ \x -> calcSize handle explorer n p x

-- runExperiment3 :: Handle -> String -> IO ()
-- runExpriment3 dir = pathWalk dir $ \file -> do
  -- Get files in the directory
  -- For each file, run the program and measure the size of the CMap and ExecEnv
  -- Write the results to a file

runExperiments :: IO ()
runExperiments = do
  -- Test multiple probabilities on lots of sizes
  -- handle <- openFile "../results/data/data1.csv" WriteMode
  -- hPutStrLn handle "N,P,X,Cmap,ExecEnv"
  -- forM_ [0..10] $ \p -> runExperiment1 handle 250 (fromIntegral p / 10)
  -- hClose handle

  -- Test multiple probability on a few sizes
  handle2 <- openFile "../results/data/data_monadic3.csv" WriteMode
  hPutStrLn handle2 "N,P,Size"
  forM_ [0..10] $ \p -> runExperiment2 handle2 200 (fromIntegral p / 10)
  hClose handle2

  -- runExperiment3 "../results/data/data3.csv"

-- Given N and P, generate a tree and measure its size
-- measureExplorer :: Int -> Float -> IO()
-- measureExplorer n p = do
--   explorer <- randomTree n p
--   size1 <- recursiveSizeNF (EM.cmap explorer)
--   size2 <- recursiveSizeNF (EM.parents explorer)
--   size3 <- recursiveSizeNF (EM.children explorer)
--   print $ "CMap size: " ++ show size1
--   print $ "ExecEnv size: " ++ show (size2 + size3)
--   print $ "Total size: " ++ show (size1 + size2 + size3)

main :: IO ()
main = do
  runExperiments


-- While rpel
-- repl = R.repl (const "While> ") parser ":" R.metaTable  (\_ ex -> return ex) (\_ -> return ()) (EM.mkExplorerNoSharing definterpM initialConfig)

replParser :: String -> p -> Either String Phrase
replParser s _ = parser s

-- Java repl
javaREPL = repl (const "Java> ") replParser ":" metaTable (\_ ex -> return ex) (putStr . concat) explorer

instance ExplorerPostValue Phrase Context [String]

explorer = mkExplorerNoSharing Pi.runPhrase initialContext

fromFile :: String -> IO ()
fromFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  case parse parsePhrase (lexer lexerSettings contents) of
    [] -> return ()
    (x : _) -> do
      (Just ctx, out) <- Pi.runPhrase x initialContext
      print $ env ctx
      print $ store ctx
      putStrLn (concat out)

measureFromFile :: String -> IO ()
measureFromFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  let explr = mkExplorerNoSharing Pi.runPhrase initialContext
  case parse parsePhrase (lexer lexerSettings contents) of
    [] -> return ()
    (x : _) -> do
      (newExplr, _) <- EM.execute x explr
      size1 <- recursiveSizeNF (EM.cmap newExplr)
      -- size2 <- recursiveSizeNF (EM.execEnv newExplr)
      print $ (size1) --, size2)

factorial :: String
factorial =
  "class Factorial{\
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
  putStr (concat out)
  return ctx

initialVars :: Gen Phrase
initialVars = do
  intAssigns <- traverse genIntInit ['a'..'j']
  boolAssigns <- traverse genBoolInit ['k'..'t']
  return $ foldr1 PSeq (intAssigns ++ boolAssigns)
  where
    genIntInit c = do
      val <- arbitrary
      return $ PSeq
        (PVar (VarDecl TInt [c]))
        (PStmt (Assign [c] (Integer val)))

    genBoolInit c = do
      val <- arbitrary
      return $ PSeq
        (PVar (VarDecl TBoolean [c]))
        (PStmt (Assign [c] (Boolean val)))

initialiseContext :: IO Context
initialiseContext = do
  vars <- generate initialVars
  runCommand vars initialContext

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
  explr <- randomTree (n - 1) p
  randPhrase <- generate (arbitrary :: Gen Phrase)
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM.currRef explr - 1))
        case EM.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM.execute randPhrase explr'
  return newExplr
