module Main where

import Abs (Expr (..), genExprValid)
import Control.Monad (foldM, forM, forM_, unless, when)
import Criterion.Main
  ( bench,
    bgroup,
    defaultConfig,
    defaultMainWith,
    env,
    nfIO,
    whnfIO,
  )
import Criterion.Types (Config (timeLimit), Verbosity (Verbose), resamples, verbosity)
import Data.Char (isAlpha, isDigit)
import Data.Tree (drawTree)
import GHC.DataSize (recursiveSizeNF)
import GLL.Combinators as C
import Interpreter (Context, initialContext, runExpr)
import qualified Language.Explorer.Compressed as EM6
import qualified Language.Explorer.Disk as EM7
import Language.Explorer.Monadic as EM1
import qualified Language.Explorer.Monadic2 as EM2
import qualified Language.Explorer.Monadic3 as EM3
import qualified Language.Explorer.Monadic4 as EM4
import qualified Language.Explorer.Monadic5 as EM5
import Language.Explorer.Tools.REPL (metaTable, repl)
import qualified Language.Explorer.Tools.REPL2 as REPL2
import System.IO
  ( Handle,
    IOMode (AppendMode, WriteMode),
    hClose,
    hPutStrLn,
    openFile,
  )
import System.PosixCompat (fileSize, getFileStatus)
import Test.QuickCheck (choose, generate)
import Text.Regex.Applicative as RE (Alternative (many), psym)

type Parser a = BNF Token a

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
      <||> Symbol <$$> id_lit
      <||> String <$$> string_lit
      <||> parens (Lambda <$$> (keyword "lambda" **> parens parseIdList) C.<**> parseExpr)
      <||> List <$$> parens (multiple parseExpr)

parseIdList :: Parser [String]
parseIdList =
  "Id list" <::=> multiple id_lit

parseExprList :: Parser [Expr]
parseExprList =
  "Expr list" <::=> multiple parseExpr

lexerSettings :: LexerSettings
lexerSettings =
  emptyLanguage
    { keywords = ["true", "false", "lambda"],
      keychars = "()[]{},.;!",
      identifiers = RE.many (psym (\c -> isAlpha c || isDigit c || elem c "+-*/<=>?"))
    }

parser :: String -> Either String Expr
parser s = case lexerEither lexerSettings s of
  Left err -> Left err
  Right tok -> case parseWithOptionsAndError [maximumErrors 1] parseExpr tok of
    Left err -> Left err
    Right [] -> Left "No parse"
    Right (x : _) -> Right x

parseProgram :: String -> Maybe Expr
parseProgram s = case parser s of
  Left _ -> Nothing
  Right e -> Just e

replParser :: String -> p -> Either String Expr
replParser s _ = parser s

schemeExplorer :: Explorer Expr IO Context [String]
schemeExplorer = EM1.mkExplorerNoSharing runExpr initialContext

schemeREPL :: IO ()
schemeREPL = repl (const "Scheme> ") replParser ":" metaTable (\_ ex -> return ex) (putStr . concat) schemeExplorer

schemeREPL2 :: IO ()
schemeREPL2 = do
  let prompt = const "Scheme> "
  let metaPrefix = ":"
  let metaHandler _ _ = return ()
  let outputHandler = putStr . concat
  explorer <- EM7.mkExplorerIO EM7.defaultSettings "scheme.db" runExpr initialContext
  REPL2.repl prompt replParser metaPrefix REPL2.metaTable metaHandler outputHandler explorer

initialContext' :: IO Context
initialContext' = return initialContext

-- TESTING
printExplorer :: Explorer Expr IO Context [String] -> IO ()
printExplorer ex = putStrLn . drawTree $ show . fst <$> toTree ex

randomTree :: Int -> Float -> IO (Explorer Expr IO Context [String])
randomTree 1 _ = EM1.mkExplorerNoSharing runExpr <$> initialContext'
randomTree n p = do
  explr <- randomTree (n - 1) p
  randExpr <- generate genExprValid
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM1.currRef explr - 1))
        case EM1.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM1.execute randExpr explr'
  return newExplr

calcSize :: Handle -> Explorer Expr IO Context [String] -> Int -> Float -> Int -> IO ()
calcSize handle explorer n p x = do
  print (n, p, x)
  size1 <- recursiveSizeNF (EM1.cmap explorer)
  size2 <- recursiveSizeNF (EM1.execEnv explorer)
  hPutStrLn handle $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size1 ++ "," ++ show size2

runExperiment1 :: Handle -> Int -> Float -> IO ()
runExperiment1 _ 0 _ = return ()
runExperiment1 handle n p = do
  runExperiment1 handle (n - 1) p
  forM_ [1 .. 5] $ \x -> do
    explorer <- randomTree n p
    calcSize handle explorer n p x

runExperiments :: IO ()
runExperiments = do
  -- Test multiple probabilities on lots of sizes
  handle <- openFile "../results/data/data_scheme5.csv" WriteMode
  hPutStrLn handle "COMPRESSED"
  hPutStrLn handle "N,P,X,Cmap,ExecEnv"
  forM_ [(0 :: Integer) .. 10] $ \p -> runExperiment1 handle 100 (fromIntegral p / 10)
  hClose handle

runFinalExperiments :: IO ()
runFinalExperiments = do
  handles <- forM [(1 :: Integer) .. 7] $ \i -> do
    handle <- openFile ("../results/data/scheme/v" ++ show i ++ ".csv") AppendMode
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
  forM_ [(0 :: Integer) .. 10] $ \p -> runFinalExperiment handles 100 (fromIntegral p / 10)
  -- forM_ [(0::Integer)..10] $ \p -> runFinalExperiment2 handles 500 (fromIntegral p / 10)
  mapM_ hClose handles

runDiskExperiment :: IO ()
runDiskExperiment = do
  handle <- openFile "../results/data/scheme/disk.csv" WriteMode
  hPutStrLn handle "N,P,X,Filesize"
  forM_ [(0 :: Integer) .. 10] $ \p -> runDisk handle 1000 (fromIntegral p / 10)
  hClose handle
  where
    runDisk _ 0 _ = return ()
    runDisk h n p = do
      runDisk h (n - 100) p
      forM_ [(1 :: Integer) .. 5] $ \x -> do
        print (n, p, x)
        _ <- randomTreeDisk n p
        size <- fileSize <$> getFileStatus "scheme.db"
        hPutStrLn h $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size

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

    size7 <- fileSize <$> getFileStatus "scheme.db"
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

    size7 <- fileSize <$> getFileStatus "scheme.db"
    hPutStrLn (handles !! 6) $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size7

-- First benchmark: Time to execute N expressions in a row
benchmarkExecute1 :: Int -> IO Context
benchmarkExecute1 n = foldM step initialContext [1 .. n]
  where
    step ctx _ = do
      expr <- generate genExprValid
      (Just ctx', _) <- runExpr expr ctx
      return ctx'

benchmarkExecute2 :: Int -> IO (EM1.Explorer Expr IO Context [String])
benchmarkExecute2 n = foldM step (EM1.mkExplorerNoSharing runExpr initialContext) [1 .. n]
  where
    step explr _ = fst <$> (generate genExprValid >>= flip EM1.execute explr)

benchmarkExecute3 :: Int -> IO (EM5.Explorer Expr IO Context [String])
benchmarkExecute3 n = foldM step (EM5.mkExplorerNoSharing runExpr initialContext) [1 .. n]
  where
    step explr _ = fst <$> (generate genExprValid >>= flip EM5.execute explr)

benchmarkExecute4 :: Int -> IO (EM7.Explorer Expr Context [String])
benchmarkExecute4 n = do
  explr <- EM7.mkExplorerIO EM7.defaultSettings "scheme.db" runExpr initialContext
  forM_ [1 .. n] $ \_ -> do
    expr <- generate genExprValid
    EM7.execute expr explr
  return explr

benchmarkExecute5 :: Int -> IO (EM4.Explorer Expr IO Context [String])
benchmarkExecute5 n = foldM step (EM4.mkExplorerNoSharing runExpr initialContext) [1 .. n]
  where
    step explr _ = fst <$> (generate genExprValid >>= flip EM4.execute explr)

-- Second benchmark: Time to execute one expression on a tree of size N
benchmarkExecute6 :: EM1.Explorer Expr IO Context [String] -> IO (EM1.Explorer Expr IO Context [String])
benchmarkExecute6 explr = do
  expr <- generate genExprValid
  (newExplr, _) <- EM1.execute expr explr
  return newExplr

benchmarkExecute7 :: EM5.Explorer Expr IO Context [String] -> IO (EM5.Explorer Expr IO Context [String])
benchmarkExecute7 explr = do
  expr <- generate genExprValid
  (newExplr, _) <- EM5.execute expr explr
  return newExplr

benchmarkExecute8 :: EM7.Explorer Expr Context [String] -> IO (EM7.Explorer Expr Context [String])
benchmarkExecute8 explr = do
  expr <- generate genExprValid
  _ <- EM7.execute expr explr
  return explr

benchmarkExecute9 :: EM4.Explorer Expr IO Context [String] -> IO (EM4.Explorer Expr IO Context [String])
benchmarkExecute9 explr = do
  expr <- generate genExprValid
  (newExplr, _) <- EM4.execute expr explr
  return newExplr

-- Third benchmark: Time to perform a jump on a tree of size N
benchmarkJump1 :: EM1.Explorer Expr IO Context [String] -> IO (EM1.Explorer Expr IO Context [String])
benchmarkJump1 explr = do
  jumpRef <- generate (choose (1, EM1.currRef explr - 1))
  case EM1.jump jumpRef explr of
    Just newExplr -> return newExplr
    Nothing -> return explr

benchmarkJump2 :: EM5.Explorer Expr IO Context [String] -> IO (EM5.Explorer Expr IO Context [String])
benchmarkJump2 explr = do
  jumpRef <- generate (choose (1, EM5.currRef explr - 1))
  case EM5.jump jumpRef explr of
    Just newExplr -> return newExplr
    Nothing -> return explr

benchmarkJump3 :: EM7.Explorer Expr Context [String] -> IO Ref
benchmarkJump3 explr = do
  curr <- EM7.getCurrRef explr
  jumpRef <- generate (choose (1, curr - 1))
  _ <- EM7.jump jumpRef explr
  EM7.getCurrRef explr

benchmarkJump4 :: EM4.Explorer Expr IO Context [String] -> IO (EM4.Explorer Expr IO Context [String])
benchmarkJump4 explr = do
  jumpRef <- generate (choose (1, EM4.currRef explr - 1))
  case EM4.jump jumpRef explr of
    Just newExplr -> return newExplr
    Nothing -> return explr

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {timeLimit = 60, resamples = 1000, verbosity = Verbose})
    [ bgroup
        "chain execute"
        [ bench "Plain"    $ nfIO   (benchmarkExecute1 n),
          bench "Default"  $ nfIO   (benchmarkExecute2 n),
          bench "Improved" $ nfIO   (benchmarkExecute3 n),
          bench "Disk"     $ whnfIO (benchmarkExecute4 n),
          bench "Patch"    $ nfIO   (benchmarkExecute5 n)
        ],
      bgroup
        "single execute"
        [ env (randomTree n p)     $ \explr -> bench "Default"  $ nfIO   (benchmarkExecute6 explr),
          env (randomTreeFive n p) $ \explr -> bench "Improved" $ nfIO   (benchmarkExecute7 explr),
          env (randomTreeDisk n p) $ \explr -> bench "Disk"     $ whnfIO (benchmarkExecute8 explr),
          env (randomTreeFour n p) $ \explr -> bench "Patch"    $ nfIO   (benchmarkExecute9 explr)
        ],
      bgroup
        "jump"
        [ env (randomTree n p)     $ \explr -> bench "Default"  $ nfIO   (benchmarkJump1 explr),
          env (randomTreeFive n p) $ \explr -> bench "Improved" $ nfIO   (benchmarkJump2 explr),
          env (randomTreeDisk n p) $ \explr -> bench "Disk"     $ whnfIO (benchmarkJump3 explr),
          env (randomTreeFour n p) $ \explr -> bench "Patch"    $ nfIO   (benchmarkJump4 explr)
        ]
    ]
  where
    n = 250
    p = 0.5

randomTreeDisk :: Int -> Float -> IO (EM7.Explorer Expr Context [String])
randomTreeDisk 1 _ = EM7.mkExplorerIO EM7.defaultSettings "scheme.db" runExpr initialContext
randomTreeDisk n p = do
  explr <- randomTreeDisk (n - 1) p
  randExpr <- generate genExprValid
  jumpCond <- generate $ choose (0.0, 1.0)
  when (jumpCond <= p) $ do
    curr <- EM7.getCurrRef explr
    jumpRef <- generate (choose (1, curr - 1))
    _ <- EM7.jump jumpRef explr
    return ()

  _ <- EM7.execute randExpr explr
  return explr

randomTreeFive :: Int -> Float -> IO (EM5.Explorer Expr IO Context [String])
randomTreeFive 1 _ = EM5.mkExplorerNoSharing runExpr <$> initialContext'
randomTreeFive n p = do
  explr <- randomTreeFive (n - 1) p
  randExpr <- generate genExprValid
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM5.currRef explr - 1))
        case EM5.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM5.execute randExpr explr'
  return newExplr

randomTreeFour :: Int -> Float -> IO (EM4.Explorer Expr IO Context [String])
randomTreeFour 1 _ = EM4.mkExplorerNoSharing runExpr <$> initialContext'
randomTreeFour n p = do
  explr <- randomTreeFour (n - 1) p
  randExpr <- generate genExprValid
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM4.currRef explr - 1))
        case EM4.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM4.execute randExpr explr'
  return newExplr

randomTrees ::
  Int ->
  Float ->
  IO
    ( EM1.Explorer Expr IO Context [String],
      EM2.Explorer Expr IO Context [String],
      EM3.Explorer Expr IO Context [String],
      EM4.Explorer Expr IO Context [String],
      EM5.Explorer Expr IO Context [String],
      EM6.Explorer Expr IO Context [String],
      EM7.Explorer Expr Context [String]
    )
randomTrees 1 _ = do
  explr7 <- EM7.mkExplorerIO EM7.defaultSettings "scheme.db" runExpr initialContext
  return
    ( EM1.mkExplorerNoSharing runExpr initialContext, -- Default
      EM2.mkExplorerNoSharing runExpr initialContext, -- Tree structure
      EM3.mkExplorerNoSharing runExpr initialContext, -- Tree structure + cmap Vector
      EM4.mkExplorerNoSharing runExpr initialContext, -- cmap with diffing
      EM5.mkExplorerNoSharing runExpr initialContext, -- Minimal tree structure
      EM6.mkExplorerNoSharing runExpr initialContext, -- Compressed
      explr7 -- Minimal tree structure + disk
    )
randomTrees n p = do
  (explr1, explr2, explr3, explr4, explr5, explr6, explr7) <- randomTrees (n - 1) p
  randExpr <- generate genExprValid
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

  (newExplr1, _) <- EM1.execute randExpr explr1'
  (newExplr2, _) <- EM2.execute randExpr explr2'
  (newExplr3, _) <- EM3.execute randExpr explr3'
  (newExplr4, _) <- EM4.execute randExpr explr4'
  (newExplr5, _) <- EM5.execute randExpr explr5'
  (newExplr6, _) <- EM6.execute randExpr explr6'
  _ <- EM7.execute randExpr explr7'

  return
    ( newExplr1,
      newExplr2,
      newExplr3,
      newExplr4,
      newExplr5,
      newExplr6,
      explr7'
    )

-- TESTING: COMPARE VERSIONS ON THE SAME TREE
randomTreeTwo :: Int -> Float -> IO (IO (EM7.Explorer Expr Context [String]), IO (EM1.Explorer Expr IO Context [String]))
randomTreeTwo 1 _ = return (EM7.mkExplorerIO (EM7.defaultSettings {EM7.checkpointInterval = 50}) "scheme.db" runExpr initialContext, EM1.mkExplorerNoSharing runExpr <$> initialContext')
randomTreeTwo n p = do
  (exp1, exp2) <- randomTreeTwo (n - 1) p
  explr <- exp1
  explr2 <- exp2
  randExpr <- generate genExprValid
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

  _ <- EM7.execute randExpr explr'
  (newExplr2, _) <- EM1.execute randExpr explr2'

  return (return explr, return newExplr2)

measureExplorerTwo :: Int -> Float -> IO ()
measureExplorerTwo n p = do
  (tree, tree1) <- randomTreeTwo n p
  explr_disk <- tree
  explr1 <- tree1

  equal <- compareTrees explr_disk explr1
  unless equal $ fail "Trees are not equal"
  size2 <- fileSize <$> getFileStatus "scheme.db"
  size3 <- recursiveSizeNF (EM1.execEnv explr1)
  size4 <- recursiveSizeNF (EM1.cmap explr1)

  putStrLn "         Disk  vs  Memory"
  putStrLn $ "Total:   " ++ show size2 ++ " vs " ++ show (size3 + size4)

  let total1 = toInteger size2
  let total2 = toInteger size3 + toInteger size4

  if total1 < total2
    then putStrLn "Disk is smaller"
    else putStrLn "Memory is smaller"

compareTrees :: EM7.Explorer Expr Context [String] -> EM1.Explorer Expr IO Context [String] -> IO Bool
compareTrees explr1 explr2 = do
  tree1 <- EM7.toTree explr1
  let tree2 = EM1.toTree explr2
  return $ tree1 == tree2
