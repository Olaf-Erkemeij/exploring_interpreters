{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Explorer.Monadic5
  ( Explorer,
    mkExplorer,
    mkExplorerNoSharing,
    execute,
    executeAll,
    revert,
    jump,
    toTree,
    incomingEdges,
    config,
    currRef,
    leaves,
    history,
    Ref,
    Language,
    deref,
    getTrace,
    getTraces,
    getPathsFromTo,
    getPathFromTo,
    executionGraph,
    initialRef,
    fromExport,
    toExport,
  )
where

import Control.DeepSeq (NFData, rnf)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldlM)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Tree (Tree (..))

type Ref = Int

type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

data ExpNode p c o = ExpNode
  { nodeConfig :: !c,
    nodeParent :: !Ref,
    nodeEdge :: !(Maybe (p, o))
  }

instance (NFData c, NFData p, NFData o) => NFData (ExpNode p c o) where
  rnf (ExpNode c p e) = rnf c `seq` rnf p `seq` rnf e

data Explorer programs m configs output where
  Explorer ::
    Language programs m configs output =>
    { defInterp :: programs -> configs -> m (Maybe configs, output),
      currRef :: !Ref,
      genRef :: !Ref,
      history :: !(IntMap.IntMap (ExpNode programs configs output))
    } ->
    Explorer programs m configs output

mkExplorer :: Language p m c o => Bool -> (c -> c -> Bool) -> (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer shadow shadowEq definterp conf =
  Explorer
    { defInterp = definterp,
      genRef = 1, -- Currently generate references by increasing a counter.
      currRef = initialRef,
      history = IntMap.singleton initialRef (ExpNode conf 0 Nothing)
    }

initialRef :: Int
initialRef = 1

mkExplorerNoSharing :: Language p m c o => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorerNoSharing = mkExplorer False (\_ _ -> False)

config :: Explorer p m c o -> c
config e = nodeConfig $ fromMaybe (error "config: No current configuration found.") (IntMap.lookup (currRef e) (history e))

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r = nodeConfig <$> IntMap.lookup r (history e)

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c =
  e
    { currRef = newref,
      genRef = newref,
      history = IntMap.insert newref (ExpNode c (currRef e) (Just (p, o))) (history e)
    }
  where
    newref = genRef e + 1

updateExecEnvs :: Language p m c o => Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateExecEnvs e (p, newconf, output) = addNewPath e p output newconf

execute :: Language p m c o => p -> Explorer p m c o -> m (Explorer p m c o, o)
execute p e =
  do
    (mcfg, o) <- defInterp e p (config e)
    case mcfg of
      Just cfg -> return (updateExecEnvs e (p, cfg, o), o)
      Nothing -> return (e, o)

executeAll :: Language p m c o => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
executeAll ps exp = foldlM executeCollect (exp, mempty) ps
  where
    executeCollect (exp, out) p = do
      (res, out') <- execute p exp
      return (res, out `mappend` out')

revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert targetRef e
  | targetRef == currRef e = Just e
  | otherwise =
      case findAncestryPath (currRef e) targetRef [] of
        Just nodesToDelete ->
          Just $
            e
              { history = foldl' (flip IntMap.delete) (history e) nodesToDelete,
                currRef = targetRef
              }
        _ -> Nothing
  where
    findAncestryPath :: Ref -> Ref -> [Ref] -> Maybe [Ref]
    findAncestryPath start end acc
      | start == end = Just acc
      | otherwise = case nodeParent <$> IntMap.lookup start (history e) of
          Nothing -> Nothing
          Just 0 -> Nothing
          Just parentRef -> findAncestryPath parentRef end (start : acc)

jump :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump r e = IntMap.lookup r (history e) >> Just e {currRef = r}

findChildren :: Explorer p m c o -> Ref -> [Ref]
findChildren e ref = IntMap.foldrWithKey findChild [] (history e)
  where
    findChild k node acc = if nodeParent node == ref then k : acc else acc

toTree :: Explorer p m c o -> Tree (Ref, c)
toTree e = buildNode initialRef
  where
    buildNode ref = Node (ref, nodeConfig $ fromJust (IntMap.lookup ref (history e))) (map buildNode (findChildren e ref))

getEdgeInfo :: Explorer p m c o -> Ref -> Maybe ((Ref, c), (p, o), (Ref, c))
getEdgeInfo e ref =
  case IntMap.lookup ref (history e) of
    Just nodeData ->
      case (nodeParent nodeData, nodeEdge nodeData) of
        (parentRef, Just transition) | parentRef > 0 ->
          case IntMap.lookup parentRef (history e) of
            Nothing -> Nothing
            Just parentNodeData ->
              Just ((parentRef, nodeConfig parentNodeData), transition, (ref, nodeConfig nodeData))
        _ -> Nothing
    Nothing -> Nothing

incomingEdges :: Explorer p m c o -> Ref -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges e = maybeToList . getEdgeInfo e

getTrace :: Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
getTrace e = getPathFromTo e initialRef (currRef e)

getTraces :: Explorer p m c o -> [[((Ref, c), (p, o), (Ref, c))]]
getTraces e = getPathsFromTo e initialRef (currRef e)

getPathsFromTo :: Explorer p m c o -> Ref -> Ref -> [[((Ref, c), (p, o), (Ref, c))]]
getPathsFromTo exp from to = [getPathFromTo exp from to]

getPathFromTo :: Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo exp from to = reverse $ go from []
  where
    go ref acc
      | ref == to = acc
      | otherwise = case getEdgeInfo exp ref of
          Just edgeInfo@((x, _), _, _) -> go x $ edgeInfo : acc
          Nothing -> []

nodes :: Explorer p m c o -> [(Ref, c)]
nodes = map (second nodeConfig) . IntMap.assocs . history

executionGraph :: Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph e =
  ( (currRef e, config e),
    nodes e,
    mapMaybe (getEdgeInfo e) (IntMap.keys (history e))
  )

leaves :: Explorer p m c o -> [(Ref, c)]
leaves e = filter (\(ref, _) -> not (Set.member ref parents)) (nodes e)
  where
    parents = Set.fromList $ map nodeParent (IntMap.elems (history e))

toExport :: Explorer p m c o -> (Ref, IntMap.IntMap (ExpNode p c o), Ref)
toExport e = (currRef e, history e, genRef e)

fromExport :: Explorer p m c o -> (Ref, IntMap.IntMap (ExpNode p c o), Ref) -> Explorer p m c o
fromExport templateExplorer (savedCurrRef, savedHistory, savedGenRef) =
  templateExplorer
    { currRef = savedCurrRef,
      history = savedHistory,
      genRef = savedGenRef
    }
