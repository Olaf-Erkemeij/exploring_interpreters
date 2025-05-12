{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Abs
import Control.DeepSeq
import Control.Monad
import Criterion.Main as CM
import Criterion.Types (Config (timeLimit))
import Data.Functor
import qualified Data.Map as M
import Data.Tree (drawTree)
import GHC.DataSize
import GLL.Combinators
import Interpreter
import qualified Language.Explorer.Compressed as EM6
import qualified Language.Explorer.Disk as EM7
import Language.Explorer.Monadic as EM1
import qualified Language.Explorer.Monadic2 as EM2
import qualified Language.Explorer.Monadic3 as EM3
import qualified Language.Explorer.Monadic4 as EM4
import qualified Language.Explorer.Monadic5 as EM5
import Language.Explorer.Tools.Protocol
import Language.Explorer.Tools.REPL
import qualified SeqInterpreter as Pi
import System.Directory
import System.IO
import System.PosixCompat (fileSize, getFileStatus)
import Test.QuickCheck hiding (within)
import Text.Read.Lex (Lexeme (Ident))

genIntId :: Gen String
genIntId = (: []) <$> choose ('a', 'j')

genBoolId :: Gen String
genBoolId = (: []) <$> choose ('k', 't')

-- Generate an expression that reduces to a boolean
genBoolExpr :: Int -> Gen Expr
genBoolExpr 0 =
  oneof
    [ Boolean <$> arbitrary,
      Identifier <$> genBoolId
    ]
genBoolExpr n =
  oneof
    [ Boolean <$> arbitrary,
      Identifier <$> genBoolId,
      Band <$> sub n <*> sub n,
      LTh <$> subInt n <*> subInt n
    ]
  where
    sub = genBoolExpr . (`div` 4)
    subInt = genIntExpr . (`div` 4)

-- Generate an expression that reduces to an integer
genIntExpr :: Int -> Gen Expr
genIntExpr 0 =
  oneof
    [ Integer <$> arbitrary,
      Identifier <$> genIntId
    ]
genIntExpr n =
  oneof
    [ Integer <$> arbitrary,
      Identifier <$> genIntId,
      Add <$> sub n <*> sub n,
      Mul <$> sub n <*> sub n,
      Sub <$> sub n <*> sub n
    ]
  where
    sub = genIntExpr . (`div` 4)

instance Arbitrary Expr where
  arbitrary = sized $ \n -> oneof [genBoolExpr n, genIntExpr n]

instance Arbitrary Statement where
  arbitrary = sized stmt
    where
      stmt 0 = oneof [assgn, prnt]
      stmt n =
        oneof
          [ Block <$> listOf (stmt 0),
            If <$> genBoolExpr (n `div` 4) <*> stmt (n `div` 4) <*> stmt (n `div` 4),
            prnt,
            assgn
          ]
      assgn =
        oneof
          [ Assign <$> genIntId <*> (Integer <$> arbitrary),
            Assign <$> genBoolId <*> (Boolean <$> arbitrary)
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

main :: IO ()
-- main = runFinalExperiments
main =
  defaultMainWith
    (defaultConfig {timeLimit = 30})
    [ bgroup
        "chain execute"
        [ bench "Plain" $ nfIO (benchmarkExecute1 n),
          bench "Default" $ nfIO (benchmarkExecute2 n),
          bench "Improved" $ nfIO (benchmarkExecute3 n),
          bench "Disk" $ whnfIO (benchmarkExecute4 n)
        ],
      bgroup
        "single execute"
        [ CM.env (randomTree n p) $ \explr -> bench "Default" $ nfIO (benchmarkExecute5 explr),
          CM.env (randomTreeFive n p) $ \explr -> bench "Improved" $ nfIO (benchmarkExecute6 explr),
          CM.env (randomTreeDisk n p) $ \explr -> bench "Disk" $ whnfIO (benchmarkExecute7 explr)
        ],
      bgroup
        "jump"
        [ CM.env (randomTree n p) $ \explr -> bench "Default" $ nfIO (benchmarkJump1 explr),
          CM.env (randomTreeFive n p) $ \explr -> bench "Improved" $ nfIO (benchmarkJump2 explr),
          CM.env (randomTreeDisk n p) $ \explr -> bench "Disk" $ whnfIO (benchmarkJump3 explr)
        ]
    ]
  where
    n = 250
    p = 0.5

replParser :: String -> p -> Either String Phrase
replParser s _ = parser s

-- Java repl
javaREPL :: IO ()
javaREPL = repl (const "Java> ") replParser ":" metaTable (\_ ex -> return ex) (putStr . concat) explorer

instance ExplorerPostValue Phrase Context [String]

explorer :: Explorer Phrase IO Context [String]
explorer = mkExplorerNoSharing Pi.runPhrase initialContext

fromFile :: String -> IO ()
fromFile file = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  case parse parsePhrase (lexer lexerSettings contents) of
    [] -> return ()
    (x : _) -> do
      (Just ctx, out) <- Pi.runPhrase x initialContext
      print $ Interpreter.env ctx
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
      (newExplr, _) <- EM1.execute x explr
      size1 <- recursiveSizeNF (EM1.cmap newExplr)
      size2 <- recursiveSizeNF (EM1.execEnv newExplr)
      print (size1, size2)
      print $ length $ EM1.toTree newExplr

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
  intAssigns <- traverse genIntInit ['a' .. 'j']
  boolAssigns <- traverse genBoolInit ['k' .. 't']
  return $ foldr1 PSeq (intAssigns ++ boolAssigns)
  where
    genIntInit c = do
      val <- arbitrary
      return $
        PSeq
          (PVar (VarDecl TInt [c]))
          (PStmt (Assign [c] (Integer val)))

    genBoolInit c = do
      val <- arbitrary
      return $
        PSeq
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
  print $ Interpreter.env ctx
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
        jumpRef <- generate (choose (1, EM1.currRef explr - 1))
        case EM1.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM1.execute randPhrase explr'
  return newExplr

randomTreeDisk :: Int -> Float -> IO (EM7.Explorer Phrase Context [String])
randomTreeDisk 1 _ = do
  ctx <- initialiseContext
  EM7.mkExplorerIO EM7.defaultSettings "mini-java.db" Pi.runPhrase ctx
randomTreeDisk n p = do
  explr <- randomTreeDisk (n - 1) p
  randPhrase <- generate (arbitrary :: Gen Phrase)
  jumpCond <- generate $ choose (0.0, 1.0)
  when (jumpCond <= p) $ do
    curr <- EM7.getCurrRef explr
    jumpRef <- generate (choose (1, curr - 1))
    _ <- EM7.jump jumpRef explr
    return ()

  _ <- EM7.execute randPhrase explr
  return explr

randomTreeFive :: Int -> Float -> IO (EM5.Explorer Phrase IO Context [String])
randomTreeFive 1 _ = EM5.mkExplorerNoSharing Pi.runPhrase <$> initialiseContext
randomTreeFive n p = do
  explr <- randomTreeFive (n - 1) p
  randPhrase <- generate (arbitrary :: Gen Phrase)
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM5.currRef explr - 1))
        case EM5.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM5.execute randPhrase explr'
  return newExplr

randomTrees ::
  Int ->
  Float ->
  IO
    ( EM1.Explorer Phrase IO Context [String],
      EM2.Explorer Phrase IO Context [String],
      EM3.Explorer Phrase IO Context [String],
      EM4.Explorer Phrase IO Context [String],
      EM5.Explorer Phrase IO Context [String],
      EM6.Explorer Phrase IO Context [String],
      EM7.Explorer Phrase Context [String]
    )
randomTrees 1 _ = do
  context <- initialiseContext
  explr7 <- EM7.mkExplorerIO EM7.defaultSettings "mini-java.db" Pi.runPhrase context
  return
    ( EM1.mkExplorerNoSharing Pi.runPhrase context, -- Default
      EM2.mkExplorerNoSharing Pi.runPhrase context, -- Tree structure
      EM3.mkExplorerNoSharing Pi.runPhrase context, -- Tree structure + cmap Vector
      EM4.mkExplorerNoSharing Pi.runPhrase context, -- cmap with diffing
      EM5.mkExplorerNoSharing Pi.runPhrase context, -- Minimal tree structure
      EM6.mkExplorerNoSharing Pi.runPhrase context, -- Compressed
      explr7 -- Minimal tree structure + disk
    )
randomTrees n p = do
  (explr1, explr2, explr3, explr4, explr5, explr6, explr7) <- randomTrees (n - 1) p
  randPhrase <- generate (arbitrary :: Gen Phrase)
  jumpCond <- generate $ choose (0.0, 1.0)
  (explr1', explr2', explr3', explr4', explr5', explr6', explr7') <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM1.currRef explr1 - 1))
        let jumped1 = EM1.jump jumpRef explr1
            jumped2 = EM2.jump jumpRef explr2
            jumped3 = EM3.jump jumpRef explr3
            jumped4 = EM4.jump jumpRef explr4
            jumped5 = EM5.jump jumpRef explr5
            jumped6 = EM6.jump jumpRef explr6
        jumped7 <- EM7.jump jumpRef explr7
        case (jumped1, jumped2, jumped3, jumped4, jumped5, jumped6, jumped7) of
          (Just newExplr1, Just newExplr2, Just newExplr3, Just newExplr4, Just newExplr5, Just newExplr6, True) ->
            return (newExplr1, newExplr2, newExplr3, newExplr4, newExplr5, newExplr6, explr7)
          _ -> return (explr1, explr2, explr3, explr4, explr5, explr6, explr7)
      else return (explr1, explr2, explr3, explr4, explr5, explr6, explr7)

  (newExplr1, _) <- EM1.execute randPhrase explr1'
  (newExplr2, _) <- EM2.execute randPhrase explr2'
  (newExplr3, _) <- EM3.execute randPhrase explr3'
  (newExplr4, _) <- EM4.execute randPhrase explr4'
  (newExplr5, _) <- EM5.execute randPhrase explr5'
  (newExplr6, _) <- EM6.execute randPhrase explr6'
  _ <- EM7.execute randPhrase explr7'

  return
    ( newExplr1,
      newExplr2,
      newExplr3,
      newExplr4,
      newExplr5,
      newExplr6,
      explr7'
    )

-- Experiments
runFinalExperiments :: IO ()
runFinalExperiments = do
  handles <- forM [(i, j) | j <- [1 .. 2], i <- [1 .. 7]] $ \(i, j) -> do
    let base = if j == 2 then "_big" else ""
    handle <- openFile ("../results/data/mini-java/v" ++ show i ++ base ++ ".csv") WriteMode
    when (i == 1) $ do
      hPutStrLn handle "N,P,X,Cmap,ExecEnv"
    when (i == 2) $ do
      hPutStrLn handle "N,P,X,Cmap,Parents,Children"
    when (i == 3) $ do
      hPutStrLn handle "N,P,X,Cmap,Parents,Children"
    when (i == 4) $ do
      hPutStrLn handle "N,P,X,Cmap,ExecEnv"
    when (i == 5) $ do
      hPutStrLn handle "N,P,X,History"
    when (i == 6) $ do
      hPutStrLn handle "N,P,X,Cmap,ExecEnv"
    when (i == 7) $ do
      hPutStrLn handle "N,P,X,Filesize"
    return handle

  let small_handles = take 7 handles
  -- forM_ [(0::Integer)..10] $ \p -> runFinalExperiment small_handles 100 (fromIntegral p / 10)
  mapM_ hClose small_handles

  let big_handles = drop 7 handles
  forM_ [(0 :: Integer) .. 10] $ \p -> runFinalExperiment2 big_handles 300 (fromIntegral p / 10)
  mapM_ hClose big_handles

runFinalExperiment :: [Handle] -> Int -> Float -> IO ()
runFinalExperiment _ 0 _ = return ()
runFinalExperiment handles@[h1, h2, h3, h4, h5, h6, h7] n p = do
  runFinalExperiment handles (n - 1) p
  forM_ [(1 :: Integer) .. 5] $ \x -> do
    print (n, p, x)
    (explr1, explr2, explr3, explr4, explr5, explr6, _) <- randomTrees n p
    cmap1 <- recursiveSizeNF (EM1.cmap explr1)
    exec1 <- recursiveSizeNF (EM1.execEnv explr1)
    hPutStrLn h1 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap1 ++ "," ++ show exec1

    cmap2 <- recursiveSizeNF (EM2.cmap explr2)
    parents2 <- recursiveSizeNF (EM2.parents explr2)
    children2 <- recursiveSizeNF (EM2.children explr2)
    hPutStrLn h2 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap2 ++ "," ++ show parents2 ++ "," ++ show children2

    cmap3 <- recursiveSizeNF (EM3.cmap explr3)
    parents3 <- recursiveSizeNF (EM3.parents explr3)
    children3 <- recursiveSizeNF (EM3.children explr3)
    hPutStrLn h3 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap3 ++ "," ++ show parents3 ++ "," ++ show children3

    cmap4 <- recursiveSizeNF (EM4.cmap explr4)
    exec4 <- recursiveSizeNF (EM4.execEnv explr4)
    hPutStrLn h4 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap4 ++ "," ++ show exec4

    history5 <- recursiveSizeNF (EM5.history explr5)
    hPutStrLn h5 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show history5

    cmap6 <- recursiveSizeNF (EM6.cmap explr6)
    exec6 <- recursiveSizeNF (EM6.execEnv explr6)
    hPutStrLn h6 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap6 ++ "," ++ show exec6

    size7 <- fileSize <$> getFileStatus "mini-java.db"
    hPutStrLn h7 $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size7
runFinalExperiment _ _ _ = return ()

runFinalExperiment2 :: [Handle] -> Int -> Float -> IO ()
runFinalExperiment2 _ 0 _ = return ()
runFinalExperiment2 handles n p = do
  runFinalExperiment2 handles (n - 100) p
  forM_ [(1 :: Integer) .. 5] $ \x -> do
    print (n, p, x)
    (explr1, explr2, explr3, explr4, explr5, explr6, _) <- randomTrees n p
    cmap1 <- recursiveSizeNF (EM1.cmap explr1)
    exec1 <- recursiveSizeNF (EM1.execEnv explr1)
    hPutStrLn (head handles) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap1 ++ "," ++ show exec1

    cmap2 <- recursiveSizeNF (EM2.cmap explr2)
    parents2 <- recursiveSizeNF (EM2.parents explr2)
    children2 <- recursiveSizeNF (EM2.children explr2)
    hPutStrLn (handles !! 1) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap2 ++ "," ++ show parents2 ++ "," ++ show children2

    cmap3 <- recursiveSizeNF (EM3.cmap explr3)
    parents3 <- recursiveSizeNF (EM3.parents explr3)
    children3 <- recursiveSizeNF (EM3.children explr3)
    hPutStrLn (handles !! 2) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap3 ++ "," ++ show parents3 ++ "," ++ show children3

    cmap4 <- recursiveSizeNF (EM4.cmap explr4)
    exec4 <- recursiveSizeNF (EM4.execEnv explr4)
    hPutStrLn (handles !! 3) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap4 ++ "," ++ show exec4

    history5 <- recursiveSizeNF (EM5.history explr5)
    hPutStrLn (handles !! 4) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show history5

    cmap6 <- recursiveSizeNF (EM6.cmap explr6)
    exec6 <- recursiveSizeNF (EM6.execEnv explr6)
    hPutStrLn (handles !! 5) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show cmap6 ++ "," ++ show exec6

    size7 <- fileSize <$> getFileStatus "mini-java.db"
    hPutStrLn (handles !! 6) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size7

runFinalExperiment3 :: String -> IO ()
runFinalExperiment3 path = do
  resultHandle <- openFile "../results/data/mini-java/repl.csv" WriteMode
  hPutStrLn resultHandle "File,V1,V5,V7"
  files <- getDirectoryContents "examples/repl"
  forM_ files $ \file -> do
    unless (take 1 (reverse file) == "a") $ return ()
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    let explr1 = EM1.mkExplorerNoSharing Pi.runPhrase initialContext
    let explr5 = EM5.mkExplorerNoSharing Pi.runPhrase initialContext
    explr7 <- EM7.mkExplorerIO EM7.defaultSettings "mini-java.db" Pi.runPhrase initialContext

    case parse parsePhrase (lexer lexerSettings contents) of
      [] -> return ()
      (x : _) -> do
        (newExplr1, _) <- EM1.execute x explr1
        (newExplr5, _) <- EM5.execute x explr5
        _ <- EM7.execute x explr7

        cmap1 <- recursiveSizeNF (EM1.cmap newExplr1)
        exec1 <- recursiveSizeNF (EM1.execEnv newExplr1)
        history5 <- recursiveSizeNF (EM5.history newExplr5)
        size7 <- fileSize <$> getFileStatus "mini-java.db"

        hPutStrLn resultHandle $ show file ++ "," ++ show (cmap1 + exec1) ++ "," ++ show history5 ++ "," ++ show size7

    hClose handle
  hClose resultHandle

-- First benchmark: Time to execute N expressions in a row
benchmarkExecute1 :: Int -> IO Context
benchmarkExecute1 n = initialiseContext >>= \ctx -> foldM step ctx [1 .. n]
  where
    step ctx _ = do
      randPhrase <- generate (arbitrary :: Gen Phrase)
      (Just ctx', _) <- Pi.runPhrase randPhrase ctx
      return ctx'

benchmarkExecute2 :: Int -> IO (EM1.Explorer Phrase IO Context [String])
benchmarkExecute2 n = initialiseContext >>= \ctx -> foldM step (EM1.mkExplorerNoSharing Pi.runPhrase ctx) [1 .. n]
  where
    step explr _ = fst <$> (generate (arbitrary :: Gen Phrase) >>= flip EM1.execute explr)

benchmarkExecute3 :: Int -> IO (EM5.Explorer Phrase IO Context [String])
benchmarkExecute3 n = initialiseContext >>= \ctx -> foldM step (EM5.mkExplorerNoSharing Pi.runPhrase ctx) [1 .. n]
  where
    step explr _ = fst <$> (generate (arbitrary :: Gen Phrase) >>= flip EM5.execute explr)

benchmarkExecute4 :: Int -> IO (EM7.Explorer Phrase Context [String])
benchmarkExecute4 n = do
  ctx <- initialiseContext
  explr <- EM7.mkExplorerIO EM7.defaultSettings "mini-java.db" Pi.runPhrase ctx
  forM_ [1 .. n] $ \_ -> do
    randPhrase <- generate (arbitrary :: Gen Phrase)
    EM7.execute randPhrase explr
  return explr

-- Second benchmark: Time to execute one expression on a tree of size N
benchmarkExecute5 :: EM1.Explorer Phrase IO Context [String] -> IO (EM1.Explorer Phrase IO Context [String])
benchmarkExecute5 explr = do
  randPhrase <- generate (arbitrary :: Gen Phrase)
  (newExplr, _) <- EM1.execute randPhrase explr
  return newExplr

benchmarkExecute6 :: EM5.Explorer Phrase IO Context [String] -> IO (EM5.Explorer Phrase IO Context [String])
benchmarkExecute6 explr = do
  randPhrase <- generate (arbitrary :: Gen Phrase)
  (newExplr, _) <- EM5.execute randPhrase explr
  return newExplr

benchmarkExecute7 :: EM7.Explorer Phrase Context [String] -> IO (EM7.Explorer Phrase Context [String])
benchmarkExecute7 explr = do
  randPhrase <- generate (arbitrary :: Gen Phrase)
  _ <- EM7.execute randPhrase explr
  return explr

-- Third benchmark: Time to perform a jump on a tree of size N
benchmarkJump1 :: EM1.Explorer Phrase IO Context [String] -> IO (EM1.Explorer Phrase IO Context [String])
benchmarkJump1 explr = do
  jumpRef <- generate (choose (1, EM1.currRef explr - 1))
  case EM1.jump jumpRef explr of
    Just newExplr -> return newExplr
    Nothing -> return explr

benchmarkJump2 :: EM5.Explorer Phrase IO Context [String] -> IO (EM5.Explorer Phrase IO Context [String])
benchmarkJump2 explr = do
  jumpRef <- generate (choose (1, EM5.currRef explr - 1))
  case EM5.jump jumpRef explr of
    Just newExplr -> return newExplr
    Nothing -> return explr

benchmarkJump3 :: EM7.Explorer Phrase Context [String] -> IO EM6.Ref
benchmarkJump3 explr = do
  curr <- EM7.getCurrRef explr
  jumpRef <- generate (choose (1, curr - 1))
  _ <- EM7.jump jumpRef explr
  EM7.getCurrRef explr

-- TESTING: COMPARE VERSIONS ON THE SAME TREE
randomTreeTwo :: Int -> Float -> IO (IO (EM7.Explorer Phrase Context [String]), IO (EM1.Explorer Phrase IO Context [String]))
randomTreeTwo 1 _ = do
  ctx <- initialiseContext
  return (EM7.mkExplorerIO EM7.defaultSettings "mini-java.db" Pi.runPhrase ctx, return $ EM1.mkExplorerNoSharing Pi.runPhrase ctx)
randomTreeTwo n p = do
  (exp1, exp2) <- randomTreeTwo (n - 1) p
  explr <- exp1
  explr2 <- exp2
  randPhrase <- generate (arbitrary :: Gen Phrase)
  jumpCond <- generate $ choose (0.0, 1.0)

  (explr', explr2') <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM1.currRef explr2 - 1))
        jumped <- EM7.jump jumpRef explr
        case (jumped, EM1.jump jumpRef explr2) of
          (True, Just newExplr2) -> return (explr, newExplr2)
          _ -> return (explr, explr2)
      else return (explr, explr2)

  _ <- EM7.execute randPhrase explr'
  (newExplr2, _) <- EM1.execute randPhrase explr2'

  return (return explr, return newExplr2)

measureExplorerTwo :: Int -> Float -> IO ()
measureExplorerTwo n p = do
  (tree, tree1) <- randomTreeTwo n p
  explr_disk <- tree
  explr1 <- tree1

  equal <- compareTrees explr_disk explr1
  unless equal $ fail "Trees are not equal"
  size2 <- fileSize <$> getFileStatus "mini-java.db"
  size3 <- recursiveSizeNF (EM1.execEnv explr1)
  size4 <- recursiveSizeNF (EM1.cmap explr1)

  putStrLn "         Disk  vs  Memory"
  putStrLn $ "Total:   " ++ show size2 ++ " vs " ++ show (size3 + size4)

  let total1 = toInteger size2
  let total2 = toInteger size3 + toInteger size4

  if total1 < total2
    then putStrLn "Disk is smaller"
    else putStrLn "Memory is smaller"

compareTrees :: EM7.Explorer Phrase Context [String] -> EM1.Explorer Phrase IO Context [String] -> IO Bool
compareTrees explr1 explr2 = do
  tree1 <- EM7.toTree explr1
  let tree2 = EM1.toTree explr2
  return $ tree1 == tree2
