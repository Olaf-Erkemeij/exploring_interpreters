{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Explorer.Disk where

import Control.DeepSeq (NFData, rnf)
import Control.Monad
import Data.Binary (Binary)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (Result (Error, Success))
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import qualified Data.Cache.LRU.IO as LRU
import Data.Foldable (foldlM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Tree (Tree (..))
import qualified Language.Explorer.Tools.Diff as Diff
import Language.Explorer.Tools.DiskStore (Ref)
import qualified Language.Explorer.Tools.DiskStore as DiskStore

type Language p c o = (Eq p, Eq o, Monoid o)

type Storable p c o =
  ( ToJSON c,
    FromJSON c,
    Show c,
    Binary p,
    Show p,
    Binary o,
    Show o
  )

data ExplorerSettings = ExplorerSettings
  { checkpointInterval :: !Int,
    cacheSize :: !Integer,
    compressionLevel :: !Int
  }

data ExplorerState p c o = ExplorerState
  { diskStore :: !DiskStore.DiskStoreHandles,
    cache :: !(LRU.AtomicLRU Ref (ExpNode p c o)),
    currRef :: !Ref,
    genRef :: !Ref,
    interpreter :: p -> c -> IO (Maybe c, o),
    checkpoint :: !Int,
    compression :: !Int
  }

data ExpNode p c o = ExpNode
  { nodeConfig :: !c,
    nodeParent :: !Ref,
    nodeEdge :: !(Maybe (p, o))
  }

instance (NFData c, NFData p, NFData o) => NFData (ExpNode p c o) where
  rnf (ExpNode c p e) = rnf c `seq` rnf p `seq` rnf e

newtype Explorer p c o = Explorer (IORef (ExplorerState p c o))

defaultSettings :: ExplorerSettings
defaultSettings =
  ExplorerSettings
    { checkpointInterval = 5,
      cacheSize = 10,
      compressionLevel = 3
    }

initialRef :: Int
initialRef = 1

mkExplorerIO ::
  (Language p c o, Storable p c o) =>
  ExplorerSettings ->
  FilePath ->
  (p -> c -> IO (Maybe c, o)) ->
  c ->
  IO (Explorer p c o)
mkExplorerIO settings path definterp conf = do
  diskStore <- DiskStore.initStore path True
  cache <- LRU.newAtomicLRU (Just (cacheSize settings))
  let edgeBlob = Nothing
  let configBlob = Diff.encodeJSON conf
  let configBlobCompressed = Diff.compress (compressionLevel settings) configBlob

  DiskStore.writeNodeData diskStore initialRef 0 True configBlobCompressed edgeBlob

  ref <- newIORef $ ExplorerState {
    diskStore = diskStore,
    cache = cache,
    currRef = initialRef,
    genRef = initialRef,
    interpreter = definterp,
    checkpoint = checkpointInterval settings,
    compression = compressionLevel settings
  }
  return (Explorer ref)

mkExplorerExisting ::
  (Language p c o, Storable p c o) =>
  ExplorerSettings ->
  FilePath ->
  (p -> c -> IO (Maybe c, o)) ->
  IO (Explorer p c o)
mkExplorerExisting settings path definterp = do
  diskStore <- DiskStore.initStore path False
  cache <- LRU.newAtomicLRU (Just 10)
  startRef <- fromMaybe initialRef <$> DiskStore.findHighestRef diskStore

  ref <- newIORef $ ExplorerState {
    diskStore = diskStore,
    cache = cache,
    currRef = startRef,
    genRef = startRef,
    interpreter = definterp,
    checkpoint = checkpointInterval settings,
    compression = compressionLevel settings
  }

  return (Explorer ref)

closeExplorer :: Explorer p c o -> IO ()
closeExplorer (Explorer stateRef) = readIORef stateRef >>= (DiskStore.closeStore . diskStore)

reconstruct ::
  (Storable p c o) =>
  Ref ->
  ExplorerState p c o ->
  IORef (ExplorerState p c o) ->
  IO (Maybe (ExpNode p c o))
reconstruct ref state stateRef = do
  mRawData <- DiskStore.fetchNodeData (diskStore state) ref
  case mRawData of
    Nothing -> return Nothing
    Just (parentRef, isKeyframe, cBlob, mEdgeBlob) -> do
      let mEdge = mEdgeBlob >>= (Diff.decompress >=> Diff.decodeBinary)

      mConfig <-
        if isKeyframe
          then return $ Diff.decompress cBlob >>= Diff.decodeJSON
          else do
            mParentConfig <- getConfigIO parentRef stateRef
            case mParentConfig of
              Nothing -> return Nothing
              Just cParent -> do
                let mPatched = do
                      patchBlob <- Diff.decompress cBlob
                      patchData <- Diff.decodeJSON patchBlob
                      case Diff.patchObject cParent patchData of
                        Error _ -> Nothing
                        Success cPatched -> Just cPatched
                return mPatched

      let buildNode c edge = do
            let node = ExpNode c parentRef edge
            LRU.insert ref node (cache state)
            return $ Just node

      -- Combine results
      case (mConfig, mEdge) of
        (Just c, Just edge) -> buildNode c edge
        (Just c, Nothing) | ref == initialRef -> buildNode c Nothing
        _ -> return Nothing

getNodeIO :: (Storable p c o) => Ref -> IORef (ExplorerState p c o) -> IO (Maybe (ExpNode p c o))
getNodeIO ref stateRef = do
  state <- readIORef stateRef
  LRU.lookup ref (cache state) >>= maybe (reconstruct ref state stateRef) (pure . Just)

getConfigIO :: (Storable p c o) => Ref -> IORef (ExplorerState p c o) -> IO (Maybe c)
getConfigIO ref stateRef = fmap nodeConfig <$> getNodeIO ref stateRef

deref :: Storable p c o => Explorer p c o -> Ref -> IO (Maybe c)
deref (Explorer stateRef) ref = getConfigIO ref stateRef

config :: (Storable p c o) => Explorer p c o -> IO c
config (Explorer stateRef) = do
  ExplorerState {..} <- readIORef stateRef
  getConfigIO currRef stateRef >>= maybe (fail "Current configuration not found.") return

getCache :: (Storable p c o) => Explorer p c o -> IO (LRU.AtomicLRU Ref (ExpNode p c o))
getCache (Explorer stateRef) = do
  ExplorerState {..} <- readIORef stateRef
  return cache

getCurrRef :: (Storable p c o) => Explorer p c o -> IO Ref
getCurrRef (Explorer stateRef) = do
  ExplorerState {..} <- readIORef stateRef
  return currRef

execute :: (Storable p c o) => p -> Explorer p c o -> IO o
execute p (Explorer stateRef) = do
  state@ExplorerState {..} <- readIORef stateRef
  mConfig <- getConfigIO currRef stateRef
  case mConfig of
    Nothing -> error "Configuration not found."
    Just conf -> do
      (mcfg, o) <- interpreter p conf
      case mcfg of
        Nothing -> return o
        Just newconf -> do
          let newRef = genRef + 1
          let parent = currRef
          let isCheckpoint = (newRef - initialRef) `mod` checkpoint == 0
          let edgeBlob = Just (Diff.compress compression . Diff.encodeBinary $ Just (p, o))

          let configBlob = if isCheckpoint
              then Diff.compress compression . Diff.encodeJSON $ newconf
              else Diff.compress compression . Diff.encodeJSON $ Diff.computeDiff conf newconf

          DiskStore.writeNodeData diskStore newRef parent isCheckpoint configBlob edgeBlob

          let node = ExpNode newconf parent (Just (p, o))
          LRU.insert newRef node cache

          atomicModifyIORef' stateRef $ \s -> (s {currRef = newRef, genRef = newRef}, o)

executeAll :: (Storable p c o, Monoid o) => [p] -> Explorer p c o -> IO o
executeAll ps explorer = foldM (\acc p -> (acc <>) <$> execute p explorer) mempty ps

revert :: (Storable p c o) => Ref -> Explorer p c o -> IO Bool
revert targetRef (Explorer stateRef) = do
  state@ExplorerState {..} <- readIORef stateRef
  if targetRef == currRef
    then return True
    else do
      mPath <- findAncestryPath currRef targetRef [] stateRef
      case mPath of
        Nothing -> return False
        Just nodesToDelete -> do
          DiskStore.deleteNodes diskStore nodesToDelete
          forM_ nodesToDelete (`LRU.delete` cache)
          atomicModifyIORef' stateRef $ \s -> (s {currRef = targetRef}, ())
          return True

findAncestryPath :: (Storable p c o) => Ref -> Ref -> [Ref] -> IORef (ExplorerState p c o) -> IO (Maybe [Ref])
findAncestryPath start end acc stateRef
  | start == end = return $ Just acc
  | start == 0 = return Nothing
  | otherwise =
      getNodeIO start stateRef >>= \case
        Nothing -> pure Nothing
        Just node -> findAncestryPath (nodeParent node) end (start : acc) stateRef

jump :: (Storable p c o) => Ref -> Explorer p c o -> IO Bool
jump targetRef (Explorer stateRef) = do
  state@ExplorerState {..} <- readIORef stateRef
  if targetRef == currRef
    then return True
    else
      getNodeIO targetRef stateRef >>= \case
        Nothing -> return False
        Just _ -> do
          atomicModifyIORef' stateRef $ \s -> (s {currRef = targetRef}, ())
          return True

toTree :: (Storable p c o) => Explorer p c o -> IO (Tree (Ref, c))
toTree exp@(Explorer stateRef) = do
  ExplorerState {..} <- readIORef stateRef
  let buildNodeIO ref =
        getNodeIO ref stateRef >>= \case
          Nothing -> error $ "toTree: Cannot find node for ref " ++ show ref
          Just node -> do
            childTrees <- mapM buildNodeIO =<< DiskStore.findChildren diskStore ref
            return $ Node (ref, nodeConfig node) childTrees
  buildNodeIO initialRef
