module Main where

import GLL.Combinators as C
-- import Interpreter
-- import JSON
import Abs
import Language.Explorer.Monadic as EM
import Language.Explorer.Tools.Protocol
import Language.Explorer.Tools.REPL
import Data.Char (isDigit, isAlpha, isLower)
import Text.Regex.Applicative as RE
import Interpreter
import Interpreter (initialContext)

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
      <||> Lambda <$$> (keychar '(' **> keyword "lambda" **> parens parseIdList) C.<**> parseExpr <** keychar ')'
      <||> List <$$> parens (multiple parseExpr)
      -- <||> Lambda <$$> (keychar '(' **> keyword "lambda" **> parseIdList <** keychar ')' <** keychar '(' **> parseExprList <** keychar ')')

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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
