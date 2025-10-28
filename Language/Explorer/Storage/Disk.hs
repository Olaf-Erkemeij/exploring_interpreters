{-# LANGUAGE LambdaCase, ConstraintKinds #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances #-}

module Language.Explorer.Storage.Disk where 


import Language.Explorer.Storage 
import qualified Language.Explorer.Tools.DiskStore as DiskStore
import qualified Data.Cache.LRU.IO as LRU
import qualified Language.Explorer.Tools.Diff as Diff
import Control.DeepSeq (NFData, rnf)
import Control.Monad.State
import Data.Aeson
import Data.Maybe
import Control.Monad
import Data.Binary (Binary)


data ExplorerSettings = ExplorerSettings
  { checkpointInterval :: !Int,
    cacheSize :: !Integer,
    compressionLevel :: !Int
  }

data Store p c o = Store
  { diskStore :: !DiskStore.DiskStoreHandles,
    cache :: !(LRU.AtomicLRU Ref (ExpNode p c o)),
    genRef :: !Ref,
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


-- -- Needed for benchmarking, not implemented
-- instance (NFData c, NFData p, NFData o) => NFData (Explorer p c o) where
--   rnf (Explorer stateRef) = ()


defaultSettings :: ExplorerSettings
defaultSettings =
  ExplorerSettings
    { checkpointInterval = 5,
      cacheSize = 10,
      compressionLevel = 3
    }


initialRef = 1

type Storable p c o =
  ( ToJSON c,
    FromJSON c,
    Show c,
    Show p,
    Show o,
    Binary p,
    Binary o
  )


getNodeIO :: (Storable p c o) => Ref -> Store p c o -> IO (Maybe (ExpNode p c o))
getNodeIO ref state = do
  LRU.lookup ref (cache state) >>= maybe (reconstruct ref state) (pure . Just)

getConfigIO :: (Storable p c o) => Ref -> Store p c o -> IO (Maybe c)
getConfigIO ref state = fmap nodeConfig <$> getNodeIO ref state

reconstruct :: (Storable p c o) => Ref -> Store p c o -> IO (Maybe (ExpNode p c o))
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


instance (Storable p c o) => Storage IO p c o Store where 
    new newconf = do 
        s <- get 
        let newRef = (genRef s) + 1 

        let configBlob = Just $ Diff.compress (compression s) . Diff.encodeJSON $ newconf
        liftIO $ DiskStore.writeNodeData (diskStore s) newRef 0 True configBlob Nothing

        put $ s { genRef = newRef}
        return newRef

    query ref = do 
        s <- get
        m <- liftIO $ getNodeIO ref s
        case m of 
            (Just n) -> return . Just $ nodeConfig n
            Nothing -> return Nothing


    link = undefined 
    unlink = undefined 
    paths = undefined

