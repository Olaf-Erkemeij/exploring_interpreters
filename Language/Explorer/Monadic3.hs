{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Language.Explorer.Monadic3
  ( Explorer,
    mkExplorer,
    mkExplorerNoSharing,
    execute,
    executeAll,
    revert,
    jump,
    toTree,
    config,
    currRef,
    leaves,
    cmap,
    parents,
    children,
    Ref,
    Language,
    deref,
    getTrace,
    getPathFromTo,
    executionGraph,
    initialRef,
    fromExport,
    toExport,
  )
where

import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Graph.Inductive.Graph (out)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust, mapMaybe)
import Data.Tree (Tree (..))
import qualified Data.Vector as V

type Ref = Int

type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

data Explorer programs m configs output where
  Explorer ::
    Language programs m configs output =>
    { defInterp :: programs -> configs -> m (Maybe configs, output),
      config :: !configs,
      currRef :: !Ref,
      genRef :: !Ref,
      cmap :: !(V.Vector (Maybe configs)),
      parents :: !(IntMap.IntMap (Ref, (programs, output))),
      children :: !(IntMap.IntMap [(Ref, (programs, output))]),
      configEq :: configs -> configs -> Bool
    } ->
    Explorer programs m configs output

mkExplorer :: Language p m c o => Bool -> (c -> c -> Bool) -> (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer shadow shadowEq definterp conf =
  Explorer
    { defInterp = definterp,
      config = conf,
      genRef = 1, -- Currently generate references by increasing a counter.
      currRef = initialRef,
      cmap = V.singleton (Just conf),
      parents = IntMap.empty,
      children = IntMap.empty,
      configEq = shadowEq
    }

initialRef :: Int
initialRef = 1

mkExplorerNoSharing :: Language p m c o => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorerNoSharing = mkExplorer False (\_ _ -> False)

indexOf :: Ref -> Int
indexOf r
  | r <= 0 = error $ "Invalid reference (should be >= 1): " ++ show r
  | otherwise = r - 1

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r
  | r > 0 && indexOf r < V.length (cmap e) = cmap e V.! indexOf r
  | otherwise = Nothing

findRef :: Explorer p m c o -> c -> (c -> Bool) -> Maybe (Ref, c)
findRef e c eq = V.ifoldr f Nothing (cmap e)
  where
    f i cf a = case cf of
      Just conf -> if eq conf then Just (i + 1, conf) else a
      Nothing -> a

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c = case findRef e c (configEq e c) of
  Just (existingRef, _) ->
    e
      { currRef = existingRef,
        children = IntMap.insertWith (++) (currRef e) [(existingRef, (p, o))] (children e)
      }
  Nothing ->
    e
      { config = c,
        currRef = newRef,
        genRef = newRef,
        cmap = V.snoc (cmap e) (Just c),
        parents = IntMap.insert newRef (currRef e, (p, o)) (parents e),
        children = IntMap.insertWith (++) (currRef e) [(newRef, (p, o))] (children e)
      }
  where
    newRef = genRef e + 1

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

deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

cleanEdges :: [Ref] -> [(Ref, Ref, (p, o))] -> [(Ref, Ref, (p, o))]
cleanEdges ref = filter (\(s, t, l) -> t `notElem` ref)

data RevertableStatus = ContinueRevert | StopRevert deriving (Show)

getParent :: Explorer p m c o -> Ref -> Maybe (Ref, (p, o))
getParent e ref = IntMap.lookup ref (parents e)

findRevertableNodes :: Explorer p m c o -> Ref -> [Ref]
findRevertableNodes explr target = go (currRef explr) []
  where
    go ref path
      | ref == target = ref : path
      | otherwise = case getParent explr ref of
          Just (parent, _) -> go parent (ref : path)
          Nothing -> []

revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert target e
  | target `elem` ancestors =
      Just $
        e
          { currRef = target,
            cmap = cmap e V.// [(indexOf r, Nothing) | r <- toDelete],
            parents = IntMap.filterWithKey (\k _ -> k `notElem` toDelete) (parents e),
            children = IntMap.map (filter (\(r, _) -> r `notElem` toDelete)) (children e)
          }
  | otherwise = Nothing
  where
    toDelete = getParentChain e target
    ancestors = getParentChain e (currRef e)

getParentChain :: Explorer p m c o -> Ref -> [Ref]
getParentChain e ref = case getParent e ref of
  Just (parent, _) -> parent : getParentChain e parent
  Nothing -> [ref]

jump :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump r e = case deref e r of
  (Just c) -> return $ e {config = c, currRef = r}
  Nothing -> Nothing

toTree :: Explorer p m c o -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where
    mkTree ref = Node (ref, fromJust $ deref exp ref) (map (mkTree . fst) (fromJust $ IntMap.lookup ref (children exp)))

getTrace :: Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
getTrace e = getPathFromTo e initialRef (currRef e)

mapOut :: Explorer p m c o -> Gr Ref (p, o) -> [Ref] -> Ref -> (Ref, Ref, (p, o)) -> Maybe [[((Ref, c), (p, o), (Ref, c))]]
mapOut exp gr visited goal (s, t, (l, o))
  | goal == t = Just $ [((s, unpack s), (l, o), (t, unpack t))] : explore
  | otherwise = if t `elem` visited then Nothing else Just explore
  where
    explore = map (((s, unpack s), (l, o), (t, unpack t)) :) (concat $ mapMaybe (mapOut exp gr (t : visited) goal) (out gr t))
    unpack ref = fromJust $ deref exp ref

getPathFromTo :: Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo exp from to = reverse $ go to []
  where
    go ref visited
      | ref == from = visited
      | otherwise = case getParent exp ref of
          Just (parent, (p, o)) ->
            let fromConf = fromJust $ deref exp from
                toConf = fromJust $ deref exp to
             in go parent (((parent, fromJust $ deref exp parent), (p, o), (ref, fromJust $ deref exp ref)) : visited)
          Nothing -> []

getNodeList :: Explorer p m c o -> [(Int, c)]
getNodeList exp = [(i + 1, conf) | (i, Just conf) <- (V.toList . V.indexed) $ cmap exp]

executionGraph :: Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph exp =
  (curr, nodes, edges)
  where
    curr = (currRef exp, config exp)
    nodes = getNodeList exp
    edges =
      concatMap
        (\(p, (prog, out), c) -> [((p, fromJust $ deref exp p), (prog, out), (c, fromJust $ deref exp c))])
        [(p, l, c) | (c, (p, l)) <- IntMap.toList $ parents exp]

-- |
--  Returns all configurations that have not been the source for an execute action.
--  This corresponds to leaves in a tree or nodes without an outbound-edge in a graph.
leaves :: Explorer p m c o -> [(Ref, c)]
leaves exp = mapMaybe pair $ zip [1 ..] (V.toList (cmap exp))
  where
    pair (r, Just c) | not (r `IntMap.member` children exp) = Just (r, c)
    pair _ = Nothing

toExport :: Explorer p m c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))])
toExport exp =
  ( currRef exp,
    getNodeList exp,
    map (\(c, (p, l)) -> (p, c, l)) (IntMap.toList $ parents exp)
  )

fromExport ::
  Language p m c o =>
  Explorer p m c o ->
  (Ref, [(Ref, c)], [(Ref, Ref, (p, o))]) ->
  Explorer p m c o
fromExport exp (curr, nds, edgs) =
  exp
    { genRef = findMax nds,
      config = findCurrentConf curr nds,
      currRef = curr,
      cmap = newCmap,
      parents = IntMap.fromList [(c, (p, l)) | (p, c, l) <- edgs],
      children = IntMap.fromListWith (++) [(p, [(c, l)]) | (p, c, l) <- edgs]
    }
  where
    findMax l = maximum $ map fst l
    findCurrentConf curr nds = case lookup curr nds of
      Just conf -> conf
      Nothing -> error "no config found"
    newCmap = V.generate (maximum (map fst nds)) $ \i -> lookup (i + 1) nds
