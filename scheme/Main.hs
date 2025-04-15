module Main where

import GLL.Combinators as C
-- import Interpreter
-- import JSON
import Abs
import qualified Language.Explorer.Monadic as EM1
import Language.Explorer.Monadic as EM
import Language.Explorer.Tools.Protocol
import Language.Explorer.Tools.REPL
import Language.Explorer.Tools.Diff
import Data.Char (isDigit, isAlpha, isLower)
import Text.Regex.Applicative as RE
import Test.QuickCheck ( choose, generate )
import GHC.DataSize ( recursiveSizeNF )
import Data.Tree (drawTree)
import Control.Monad
import System.IO
import Interpreter ( runExpr, initialContext, Context )

import System.PosixCompat

import qualified Language.Explorer.Disk as ED
import qualified Language.Explorer.Tools.REPL2 as REPL2

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
      identifiers = RE.many (RE.psym (\c -> isAlpha c || isDigit c || elem c "+-*/<=>?"))
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

schemeExplorer = EM.mkExplorerNoSharing runExpr initialContext

schemeREPL :: IO ()
schemeREPL = repl (const "Scheme> ") replParser ":" metaTable (\_ ex -> return ex) (putStr . concat) schemeExplorer

schemeREPL2 :: IO ()
schemeREPL2 = do
  let prompt = const "Scheme> "
  let parser = replParser
  let metaPrefix = ":"
  let metaTable = REPL2.metaTable
  let metaHandler _ ex = return ()
  let outputHandler = putStr . concat
  explorer <- ED.mkExplorerIO ED.defaultSettings "scheme.db" runExpr initialContext
  REPL2.repl prompt parser metaPrefix metaTable metaHandler outputHandler explorer

initialContext' :: IO Context
initialContext' = return initialContext

-- TESTING
printExplorer :: Explorer Expr IO Context [String] -> IO ()
printExplorer ex = putStrLn . drawTree $ show . fst <$> toTree ex

randomTree :: Int -> Float -> IO (Explorer Expr IO Context [String])
randomTree 1 _ = EM.mkExplorerNoSharing runExpr <$> initialContext'
randomTree n p = do
  explr <- randomTree (n - 1) p
  randExpr <- generate genExprValid
  jumpCond <- generate $ choose (0.0, 1.0)
  explr' <-
    if jumpCond <= p
      then do
        jumpRef <- generate (choose (1, EM.currRef explr - 1))
        case EM.jump jumpRef explr of
          Just newExplr -> return newExplr
          Nothing -> return explr
      else return explr
  (newExplr, _) <- EM.execute randExpr explr'
  -- let Just c = getConfigRecursive newExplr (EM.currRef newExplr)
  -- unless (c == EM.config newExplr) $ error "Config mismatch"
  return newExplr

calcSize :: Handle -> Explorer Expr IO Context [String] -> Int -> Float -> Int -> IO ()
calcSize handle explorer n p x = do
  print (n, p, x)
  size1 <- recursiveSizeNF (EM.cmap explorer)
  size2 <- recursiveSizeNF (EM.execEnv explorer)
  hPutStrLn handle $ show n ++ "," ++ show p ++ "," ++ show x ++ "," ++ show size1 ++ "," ++ show size2

runExperiment1 :: Handle -> Int -> Float -> IO ()
runExperiment1 handle 0 _ = return ()
runExperiment1 handle n p = do
  runExperiment1 handle (n - 1) p
  forM_ [1..5] $ \x -> do
    explorer <- randomTree n p
    calcSize handle explorer n p x

runExperiments :: IO ()
runExperiments = do
  -- Test multiple probabilities on lots of sizes
  handle <- openFile "../results/data/data_scheme5.csv" WriteMode
  hPutStrLn handle "COMPRESSED"
  hPutStrLn handle "N,P,X,Cmap,ExecEnv"
  forM_ [0..10] $ \p -> runExperiment1 handle 100 (fromIntegral p / 10)
  hClose handle

main :: IO ()
main = runExperiments

-- Given N and P, generate a tree and measure its size
measureExplorer :: Int -> Float -> IO()
measureExplorer n p = do
  explorer <- randomTree n p
  size1 <- recursiveSizeNF (EM.cmap explorer)
  size2 <- recursiveSizeNF (EM.execEnv explorer)
  print $ "CMap size: " ++ show size1
  print $ "ExecEnv size: " ++ show size2
  print $ "Total size: " ++ show (size1 + size2)
  -- print $ "Total size: " ++ show size1

-- TESTING: COMPARE VERSIONS ON THE SAME TREE
randomTreeTwo :: Int -> Float -> IO (IO (ED.Explorer Expr Context [String]), IO (EM1.Explorer Expr IO Context [String]))
randomTreeTwo 1 _ = return (ED.mkExplorerIO ED.defaultSettings "scheme.db" runExpr initialContext, EM1.mkExplorerNoSharing runExpr <$> initialContext')
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
        jumped <- ED.jump jumpRef explr
        case (jumped, EM1.jump jumpRef explr2) of
          (True, Just newExplr2) -> return (explr, newExplr2)
          _ -> return (explr, explr2)
      else return (explr, explr2)

  _ <- ED.execute randExpr explr'
  (newExplr2, _) <- EM1.execute randExpr explr2'

  return (return explr, return newExplr2)

measureExplorerTwo :: Int -> Float -> IO()
measureExplorerTwo n p = do
  (_, tree1) <- randomTreeTwo n p
  explr1 <- tree1
  size2 <- fileSize <$> getFileStatus "scheme.db"
  size3 <- recursiveSizeNF (EM1.execEnv explr1)
  size4 <- recursiveSizeNF (EM1.cmap explr1)

  putStrLn   "         Disk  vs  Memory"
  putStrLn $ "Total:   " ++ show size2 ++ " vs " ++ show (size3 + size4)

  let total1 = toInteger size2
  let total2 = toInteger size3 + toInteger size4

  if total1 < total2
    then putStrLn "Disk is smaller"
    else putStrLn "Memory is smaller"
