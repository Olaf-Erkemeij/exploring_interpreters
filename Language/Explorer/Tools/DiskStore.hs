{-# LANGUAGE OverloadedStrings #-}

module Language.Explorer.Tools.DiskStore where

import qualified Codec.Compression.Zstd as Zstd
import Control.DeepSeq (NFData)
import Control.Exception (IOException, catch, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, Result (..), ToJSON, Value, decodeStrict', encode)
import Data.Aeson.Diff (Patch, diff, patch)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)

type Ref = Int

data DiskStoreHandles = DiskStoreHandles {db :: !Connection, path :: !FilePath}

purgeFile :: FilePath -> IO ()
purgeFile path = do
  exists <- doesFileExist path
  when exists (removeFile path)

initStore :: FilePath -> Bool -> IO DiskStoreHandles
initStore path purge = do
  when purge (purgeFile path)
  conn <- open path
  -- TODO: WAL mode?
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS History ( \
    \  ref INTEGER PRIMARY KEY,           \
    \  parent INTEGER NOT NULL,           \
    \  checkpoint BOOLEAN NOT NULL,       \
    \  config BLOB,                       \
    \  edge BLOB                          \
    \);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_parent ON History (parent);"
  return $ DiskStoreHandles conn path

closeStore :: DiskStoreHandles -> IO ()
closeStore (DiskStoreHandles conn path) = do
  close conn
  purgeFile path

writeNodeData ::
  DiskStoreHandles ->
  Ref ->
  Ref ->
  Bool ->
  Maybe B.ByteString ->
  Maybe B.ByteString ->
  IO ()
writeNodeData (DiskStoreHandles conn _) ref parent checkpoint config edge =
  execute
    conn
    "INSERT INTO History (ref, parent, checkpoint, config, edge) \
    \VALUES (?, ?, ?, ?, ?)"
    (ref, parent, checkpoint, maybe SQLNull SQLBlob config, maybe SQLNull SQLBlob edge)

fetchNodeData :: DiskStoreHandles -> Ref -> IO (Maybe (Ref, Bool, Maybe B.ByteString, Maybe B.ByteString))
fetchNodeData (DiskStoreHandles conn _) ref =
  listToMaybe
    <$> query
      conn
      "SELECT parent, checkpoint, config, edge \
      \FROM History WHERE ref = ?"
      (Only ref)

deleteNodes :: DiskStoreHandles -> [Ref] -> IO ()
deleteNodes (DiskStoreHandles conn _) refs = withTransaction conn $ do
  let placeholders = intercalate "," (replicate (length refs) "?")
  let sql = "DELETE FROM History WHERE ref IN (" ++ placeholders ++ ")"
  execute conn (Query $ T.pack sql) refs

findChildren :: DiskStoreHandles -> Ref -> IO [Ref]
findChildren (DiskStoreHandles conn _) ref = do
  rows <- query conn "SELECT ref FROM History WHERE parent = ?" (Only ref)
  return $ map fromOnly rows

findHighestRef :: DiskStoreHandles -> IO (Maybe Ref)
findHighestRef (DiskStoreHandles conn _) = do
  rows <- query_ conn "SELECT MAX(ref) FROM History"
  return $ listToMaybe $ map fromOnly rows
