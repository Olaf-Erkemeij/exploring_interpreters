{-# LANGUAGE ConstraintKinds #-}

module Language.Explorer.Tools.REPL where

import Control.Monad.IO.Class (MonadIO(..))
import Language.Explorer.Monadic
    (Explorer(config), execute, revert, jump, toTree)
import qualified System.Console.Haskeline as Hl
import Data.Tree (drawTree)
import Data.Maybe (fromJust, isNothing)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf)
import Text.Read (readMaybe)
import Control.Arrow (Arrow(first))
import Control.Monad.Trans
import Control.Monad.Catch
import Data.Binary (Binary)
import Data.Aeson (ToJSON, FromJSON)

-- type Conf c = (ToJSON c, FromJSON c, Binary c)
type Conf c = Show c
type MetaTable p m c o = [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
type RParser e p c = String -> c -> Either e p
type Prompt p m c o = Explorer p m c o -> String
type MetaHandler p m c o = String -> Explorer p m c o -> m (Explorer p m c o)
type OutputHandler m o = o -> m ()
type Repl e p m c o = Prompt p m c o -> RParser e p c -> String -> MetaTable p m c o -> MetaHandler p m c o -> OutputHandler m o -> Explorer p m c o -> m ()


handleJump :: (MonadIO m, Eq p, Eq o, Monoid o, Conf c) => String -> Explorer p m c o -> m (Explorer p m c o)
handleJump input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> case jump ref_id ex of
    (Just ex') -> return ex'
    Nothing -> liftIO $ putStrLn "Given reference is not in the exploration tree." >> return ex
  Nothing -> liftIO $ putStrLn "the jump command requires an integer argument." >> return ex

handleRevert :: (MonadIO m, Eq p, Eq o, Monoid o, Conf c) => MetaHandler p m c o
handleRevert input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> case revert ref_id ex of
    (Just ex') -> do
      liftIO $ putStrLn "reverting"
      return ex'
    Nothing -> liftIO $ putStrLn "Given reference is not valid for reverting." >> return ex
  Nothing -> liftIO $ putStrLn "the jump command requires an integer argument." >> return ex

handleTree :: (MonadIO m, Eq p, Eq o, Monoid o, Conf c) => MetaHandler p m c o
handleTree input ex = do
  liftIO $ putStrLn . drawTree $ fmap (show . fst) (toTree ex)
  return ex

metaTable :: (MonadIO m, Eq p, Eq o, Monoid o, Conf c) => [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
metaTable = [
  ("jump", handleJump),
  ("revert", handleRevert),
  ("tree", handleTree)]

constructMetaTable :: (MonadIO m, Eq p, Eq o, Monoid o, Conf c) => String -> [(String, String -> Explorer p m c o -> m (Explorer p m c o))]
constructMetaTable prefix = map (first (prefix ++ )) metaTable

repl :: (Show e, Show o, Eq p, Eq o, Monoid o, MonadIO m, MonadMask m, Conf c) => Repl e p m c o
repl prompt parser metaPrefix metaTable metaHandler outputHandler ex =
  Hl.runInputT Hl.defaultSettings (loop ex)
    where
    loop ex = do
      minput <- Hl.getInputLine . prompt $ ex
      case minput of
        (Just input) -> lift (if metaPrefix `isPrefixOf` input then runMeta input else runExec input) >>= loop
        Nothing -> return ()
      where
        runMeta input =
          let (pcmd, args) = break isSpace input in
            case find (\(cmd, _) -> (metaPrefix ++ cmd) == pcmd) metaTable of
              Just (_, f) -> f args ex
              Nothing -> metaHandler input ex
        runExec input =
          case parser input (config ex) of
            (Right program) -> execute program ex >>= \(newEx, out) -> outputHandler out >> return newEx
            (Left err) -> liftIO (print err) >> return ex
