{-# LANGUAGE ConstraintKinds #-}

module Language.Explorer.Tools.REPL2 where

import Control.Arrow (Arrow (first))
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust, isNothing)
import Data.Tree (drawTree)
import Language.Explorer.Disk
  ( Explorer,
    closeExplorer,
    config,
    execute,
    jump,
    revert,
    toTree,
  )
import qualified System.Console.Haskeline as Hl
import Text.Read (readMaybe)

type Storable p c o =
  ( ToJSON p,
    ToJSON c,
    ToJSON o,
    FromJSON p,
    FromJSON c,
    FromJSON o,
    Show p,
    Show c,
    Show o
  )

type Conf c = Show c

type MetaTable p c o = [(String, String -> Explorer p c o -> IO ())]

type RParser e p c = String -> c -> Either e p

type Prompt p c o = Explorer p c o -> String

type MetaHandler p c o = String -> Explorer p c o -> IO ()

type OutputHandler o = o -> IO ()

type Repl e p c o = Prompt p c o -> RParser e p c -> String -> MetaTable p c o -> MetaHandler p c o -> OutputHandler o -> Explorer p c o -> IO ()

handleJump :: (Eq p, Eq o, Monoid o, Storable p c o) => MetaHandler p c o
handleJump input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> do
    jumped <- jump ref_id ex
    unless jumped $ putStrLn "Given reference is not in the exploration tree."
  Nothing -> liftIO $ putStrLn "the jump command requires an integer argument."

handleRevert :: (Eq p, Eq o, Monoid o, Storable p c o) => MetaHandler p c o
handleRevert input ex = case readMaybe (dropWhile isSpace input) of
  (Just ref_id) -> do
    reverted <- revert ref_id ex
    unless reverted $ putStrLn "Given reference is not valid for reverting."
  Nothing -> putStrLn "the jump command requires an integer argument."

handleTree :: (Eq p, Eq o, Monoid o, Storable p c o) => MetaHandler p c o
handleTree input ex = do
  tree <- toTree ex
  liftIO $ putStrLn . drawTree $ fmap (show . fst) tree

metaTable :: (Eq p, Eq o, Monoid o, Storable p c o) => MetaTable p c o
metaTable =
  [ ("jump", handleJump),
    ("revert", handleRevert),
    ("tree", handleTree)
  ]

constructMetaTable :: (Eq p, Eq o, Monoid o, Storable p c o) => String -> MetaTable p c o
constructMetaTable prefix = map (first (prefix ++)) metaTable

repl :: (Storable p c o, Show e, Show o, Eq p, Eq o, Monoid o, Conf c) => Repl e p c o
repl prompt parser metaPrefix metaTable metaHandler outputHandler ex =
  Hl.runInputT Hl.defaultSettings (Hl.withInterrupt loop)
  where
    loop = Hl.handleInterrupt (Hl.outputStrLn "Exiting" >> cleanup) $ do
      minput <- Hl.getInputLine . prompt $ ex
      case minput of
        (Just input) -> liftIO (if metaPrefix `isPrefixOf` input then runMeta input else runExec input) >> loop
        Nothing -> return ()
      where
        runMeta input =
          let (pcmd, args) = break isSpace input
           in case find (\(cmd, _) -> (metaPrefix ++ cmd) == pcmd) metaTable of
                Just (_, f) -> f args ex
                Nothing -> metaHandler input ex
        runExec input = unless (null input) $ do
          conf <- config ex
          case parser input conf of
            (Right program) -> execute program ex >>= \out -> outputHandler out
            (Left err) -> liftIO (print err)
    cleanup = liftIO $ closeExplorer ex
