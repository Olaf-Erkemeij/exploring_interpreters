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
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.IO.Class

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

newtype Explorer p c o = Explorer (ExplorerState p c o)

-- Needed for benchmarking, not implemented
instance (NFData c, NFData p, NFData o) => NFData (Explorer p c o) where
  rnf (Explorer stateRef) = ()

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
  let configBlobCompressed = Just $ Diff.compress (compressionLevel settings) configBlob

  DiskStore.writeNodeData diskStore initialRef 0 True configBlobCompressed edgeBlob

  let ref = ExplorerState {
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

  let ref = ExplorerState {
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
closeExplorer (Explorer state) = DiskStore.closeStore (diskStore state)

reconstruct ::
  (Storable p c o) =>
  Ref ->
  ExplorerState p c o ->
  IO (Maybe (ExpNode p c o))
reconstruct ref state = do
  mRawData <- DiskStore.fetchNodeData (diskStore state) ref
  case mRawData of
    Nothing -> return Nothing
    Just (parentRef, isKeyframe, cBlob, mEdgeBlob) -> do
      let mEdge = mEdgeBlob >>= (Diff.decompress >=> Diff.decodeBinary)

      mConfig <-
        if isNothing cBlob
          then getConfigIO parentRef state
        else
          if isKeyframe
            then return $ Diff.decompress (fromJust cBlob) >>= Diff.decodeJSON
            else do
              mParentConfig <- getConfigIO parentRef state
              case mParentConfig of
                Nothing -> return Nothing
                Just cParent -> do
                  let mPatched = do
                        patchBlob <- Diff.decompress (fromJust cBlob)
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

getNodeIO :: (Storable p c o) => Ref -> ExplorerState p c o -> IO (Maybe (ExpNode p c o))
getNodeIO ref state = do
  LRU.lookup ref (cache state) >>= maybe (reconstruct ref state) (pure . Just)

getConfigIO :: (Storable p c o) => Ref -> ExplorerState p c o -> IO (Maybe c)
getConfigIO ref state = fmap nodeConfig <$> getNodeIO ref state

deref :: Storable p c o => Explorer p c o -> Ref -> IO (Maybe c)
deref (Explorer state) ref = getConfigIO ref state

getNode :: (Storable p c o) => Explorer p c o -> Ref -> IO (Maybe (ExpNode p c o))
getNode (Explorer stateRef) ref = getNodeIO ref stateRef

config :: (Storable p c o) => Explorer p c o -> IO c
config (Explorer state) = do
  getConfigIO (currRef state) state >>= maybe (fail "Current configuration not found.") return

getCache :: (Storable p c o) => Explorer p c o -> IO (LRU.AtomicLRU Ref (ExpNode p c o))
getCache (Explorer state) = do
  return (cache state)

getCacheContent :: (Storable p c o) => Explorer p c o -> IO [(Ref, ExpNode p c o)]
getCacheContent (Explorer state) = do
  LRU.toList (cache state)

getCurrRef :: (Storable p c o) => Explorer p c o -> IO Ref
getCurrRef (Explorer state) = do
  return (currRef state)

execute :: (Storable p c o) => p -> Explorer p c o -> IO (Explorer p c o, o)
execute p (Explorer state@ExplorerState{..}) = do
  mConfig <- getConfigIO currRef state
  case mConfig of
    Nothing -> error "Configuration not found."
    Just conf -> do
      (mcfg, o) <- interpreter p conf
      case mcfg of
        Nothing -> return (Explorer state, o)
        Just newconf -> do
          let newRef = genRef + 1
          let parent = currRef
          let isCheckpoint = (newRef - initialRef) `mod` checkpoint == 0
          let edgeBlob = Just (Diff.compress compression . Diff.encodeBinary $ Just (p, o))

          let diff = Diff.computeDiff conf newconf
          let configBlob
                | diff == mempty = Nothing
                | isCheckpoint = Just $ Diff.compress compression . Diff.encodeJSON $ newconf
                | otherwise = Just $ Diff.compress compression . Diff.encodeJSON $ diff

          DiskStore.writeNodeData diskStore newRef parent isCheckpoint configBlob edgeBlob

          let node = ExpNode newconf parent (Just (p, o))
          LRU.insert newRef node cache

          return (Explorer ( state {currRef = newRef, genRef = newRef} ), o)

executeAll :: (Storable p c o, Monoid o) => [p] -> Explorer p c o -> IO (Explorer p c o, o)
executeAll ps explorer = foldlM executeCollect (explorer, mempty) ps
  where
    executeCollect (exp, out) p = do
      (res, out') <- execute p exp
      return (res, out `mappend` out')

revert :: (Storable p c o) => Ref -> Explorer p c o -> MaybeT IO (Explorer p c o)
revert targetRef (Explorer state@ExplorerState{..}) = do
  if targetRef == currRef
    then return (Explorer state)
    else do
      mPath <- liftIO $ findAncestryPath currRef targetRef [] state
      case mPath of
        Nothing -> MaybeT (return Nothing)
        Just nodesToDelete -> do
          liftIO $ DiskStore.deleteNodes diskStore nodesToDelete
          liftIO $ forM_ nodesToDelete (`LRU.delete` cache)

          return (Explorer (state {currRef = targetRef}))

findAncestryPath :: (Storable p c o) => Ref -> Ref -> [Ref] -> ExplorerState p c o -> IO (Maybe [Ref])
findAncestryPath start end acc state
  | start == end = return $ Just acc
  | start == 0 = return Nothing
  | otherwise =
      getNodeIO start state >>= \case
        Nothing -> pure Nothing
        Just node -> findAncestryPath (nodeParent node) end (start : acc) state

jump :: (Storable p c o) => Ref -> Explorer p c o -> MaybeT IO (Explorer p c o)
jump targetRef (Explorer state@ExplorerState{..}) = do
  if targetRef == currRef
    then return (Explorer state)
    else do
      result <- liftIO $ getNodeIO targetRef state
      case result of
        Nothing -> MaybeT (return Nothing)
        Just _ -> do
          return (Explorer (state {currRef = targetRef}))

toTree :: (Storable p c o) => Explorer p c o -> IO (Tree (Ref, c))
toTree exp@(Explorer state@ExplorerState{..}) = do
  let buildNodeIO ref =
        getNodeIO ref state >>= \case
          Nothing -> error $ "toTree: Cannot find node for ref " ++ show ref
          Just node -> do
            childTrees <- mapM buildNodeIO =<< DiskStore.findChildren diskStore ref
            return $ Node (ref, nodeConfig node) childTrees
  buildNodeIO initialRef
