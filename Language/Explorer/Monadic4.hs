{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Explorer.Monadic4
    ( Explorer
    , mkExplorer
    , mkExplorerNoSharing
    , execute
    , executeAll
    , revert
    , jump
    , toTree
    , incomingEdges
    , config
    , execEnv
    , currRef
    , leaves
    , cmap
    , Ref
    , Language
    , deref
    , getTrace
    , getTraces
    , getPathsFromTo
    , getPathFromTo
    , executionGraph
    , initialRef
    , fromExport
    , toExport
    , getConfig
    , getConfigRecursive
    , showPath
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.Graph.Inductive.Query ( reachable )
import Data.Graph.Inductive.Query.BFS (esp)
import Data.Tree (Tree(..))

import qualified Data.IntMap.Strict as IntMap
import Data.List ( find, (\\) )
import Data.Foldable ( find, foldlM )
import Data.Maybe ( fromJust, mapMaybe )
import Data.Bifunctor ( Bifunctor(second) )

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, Result(..), Value)
import Data.Aeson.Diff (diff, patch, Patch)
import qualified Data.Aeson.Types as AesonTypes
import qualified Language.Explorer.Tools.Diff as DiffTool

type Ref = Int
type Language p m c o = (Eq p, Eq o, Monad m, Monoid o, ToJSON c, FromJSON c)

data ConfigDiff c = FullConfig !c
                  | PatchedConfig { baseRef :: !Ref
                                  , patchData :: !Patch
                                  , distance :: !Int
                                  }
                  deriving (Show, Eq, Generic)

instance NFData c => NFData (ConfigDiff c)

data Explorer programs m configs output where
    Explorer :: Language programs m configs output =>
        { defInterp :: programs -> configs -> m (Maybe configs, output)
        , config :: !configs
        , currRef :: !Ref
        , genRef :: !Ref
        , cmap :: IntMap.IntMap (ConfigDiff configs)
        , execEnv :: !(Gr Ref (programs, output))
        , maxPathLength :: !Int
        , maxDegree :: !Int
        } -> Explorer programs m configs output

mkExplorer :: Language p m c o => Int -> Int -> (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer maxPath maxDegree definterp conf = Explorer
    { defInterp = definterp
    , config = conf
    , currRef = initialRef
    , genRef = 1 -- Currently generate references by increasing a counter.
    , cmap = IntMap.fromList [(initialRef, FullConfig conf)]
    , execEnv = mkGraph [(initialRef, initialRef)] []
    , maxPathLength = maxPath
    , maxDegree = maxDegree
}

initialRef :: Int
initialRef = 1

mkExplorerNoSharing :: Language p m c o  => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorerNoSharing = mkExplorer 5 1 -- TODO: Is this sensible?

showPath :: (ToJSON c, FromJSON c) => Explorer p m c o -> Ref -> [Ref]
showPath e ref = case IntMap.lookup ref (cmap e) of
    Nothing -> []
    Just conf -> case conf of
        FullConfig _ -> [ref]
        PatchedConfig parentRef _ _ -> ref : showPath e parentRef

getConfig :: (ToJSON c, FromJSON c) => Explorer p m c o -> ConfigDiff c -> Maybe c
getConfig _ (FullConfig cfg) = Just cfg
getConfig e (PatchedConfig parentRef patch _) = do
    parentCfg <- getConfigRecursive e parentRef
    case DiffTool.patchObject parentCfg patch of
        Error err -> Nothing
        Success cfg -> Just cfg

getConfigRecursive :: (ToJSON c, FromJSON c) => Explorer p m c o -> Ref -> Maybe c
getConfigRecursive e ref = case IntMap.lookup ref (cmap e) of
    Nothing -> Nothing
    Just conf -> getConfig e conf

applyPatchPath :: (ToJSON c, FromJSON c) => Explorer p m c o -> c -> Path -> Maybe c
applyPatchPath e conf [] = Just conf
applyPatchPath e conf (p:ps) = lab (execEnv e) p >>= \pr -> case IntMap.lookup pr (cmap e) of
    Just (PatchedConfig _ patchData _) -> do
      case DiffTool.patchObject conf patchData of
        Error e -> error $ "Error applying patch: " ++ e
        Success newConf -> applyPatchPath e newConf ps
    Just (FullConfig _) -> if pr == 1
                          then applyPatchPath e conf ps
                          else error $ "Unexpected FullConfig at reference: " ++ show pr
    _ -> error $ "No patch found for reference: " ++ show pr

deref :: (ToJSON c, FromJSON c) => Explorer p m c o -> Ref -> Maybe c
deref e r
  | r == currRef e = Just (config e)
  | otherwise = getConfigRecursive e r

-- Based on metrics, make either a full copy or a patch
generatePatch :: (ToJSON c, FromJSON c) => Explorer p m c o -> c -> ConfigDiff c
generatePatch e c
  | dist > maxPathLength e = FullConfig c
  | outdegree > maxDegree e = FullConfig c -- TODO: Update children pathlength
  | otherwise = PatchedConfig (currRef e) (DiffTool.computeDiff (config e) c) dist
  where dist = case IntMap.lookup (currRef e) (cmap e) of
          Nothing -> 1
          Just (FullConfig _) -> 1
          Just (PatchedConfig _ _ d) -> d + 1
        outdegree = outdeg (execEnv e) (currRef e)

addNewPath :: Language p m c o => Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c = e { config = c
                       , currRef = newref
                       , genRef = newref
                       , cmap = IntMap.insert newref patched (cmap e)
                       , execEnv = insNode (newref, newref) $ insEdge (currRef e, newref, (p,o)) (execEnv e)}
     where newref = genRef e + 1
           patched = generatePatch e c

findNodeRef :: Gr [Ref] (p, o) -> Ref -> Ref
findNodeRef g r = fst $ fromJust $ find (\(_, rs) -> r `elem` rs) $ labNodes g

updateExecEnvs :: Language p m c o => Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateExecEnvs e (p, newconf, output) = addNewPath e p output newconf

execute :: Language p m c o =>  p -> Explorer p m c o -> m (Explorer p m c o, o)
execute p e =
  do (mcfg, o) <- defInterp e p (config e)
     case mcfg of
       Just cfg -> return (updateExecEnvs e (p, cfg, o), o)
       Nothing  -> return (e, o)

executeAll :: Language p m c o => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
executeAll ps exp = foldlM executeCollect (exp, mempty) ps
  where executeCollect (exp, out) p = do (res, out') <- execute p exp
                                         return (res, out `mappend` out')

deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
deleteMap xs m = foldl (flip IntMap.delete) m xs

cleanEdges :: [Ref] -> [(Ref, Ref, (p, o))] -> [(Ref, Ref, (p, o))]
cleanEdges ref = filter (\(s, t, l) -> t `notElem` ref)

data RevertableStatus = ContinueRevert | StopRevert deriving Show

findRevertableNodes :: Graph gr => gr a b -> Node -> Node -> [Int]
findRevertableNodes gr source target =
  case findNextNodeInPath gr source target of
    (Just node) -> fst $ findRevertableNodes' gr node target
    Nothing     -> []
  where
    findNextNodeInPath gr source target = find (\n -> target `elem` reachable n gr) (suc gr source)
    findRevertableNodes' gr source target
      | source == target = if outdeg gr source > 1 then ([], StopRevert) else ([source], ContinueRevert)
      | otherwise = case findNextNodeInPath gr source target of
        (Just node) -> case findRevertableNodes' gr node target of
          (res, StopRevert) -> (res, StopRevert)
          (res, ContinueRevert) -> if outdeg gr source > 1 then (res, StopRevert) else (source : res, ContinueRevert)
        Nothing -> ([], ContinueRevert)

revert :: (ToJSON c, FromJSON c) => Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert r e
  | currRef e `elem` reachNodes =
      jump r e >>= \e' -> return $ e' { execEnv = mkGraph (zip remainNodes remainNodes) $ cleanEdges reachNodes (labEdges $ execEnv e')
                                      ,  cmap = deleteMap reachNodes (cmap e')
                                      }
  | otherwise = Nothing
  where
    reachNodes = findRevertableNodes gr r (currRef e)
    remainNodes = nodes gr \\ reachNodes
    gr = execEnv e

jump :: (ToJSON c, FromJSON c) => Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump r e = case deref e r of
             (Just c) -> return $ e { config = c, currRef = r }
             Nothing -> Nothing

toTree :: (ToJSON c, FromJSON c) => Explorer p m c o -> Tree (Ref, c)
toTree exp = mkTree initialRef
  where graph = execEnv exp
        target (_, r, _) = r
        mkTree r = Node (r, (fromJust . getConfig exp) (cmap exp IntMap.! r)) (map (mkTree . target) (out graph r))

incomingEdges :: (ToJSON c, FromJSON c) => Ref -> Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
incomingEdges ref e = foldr (\(s, t, l) acc ->  ((s, unpack s), l, (t, unpack t)) : acc) [] (filter (\(_, t, _) -> t == ref) (labEdges (execEnv e)))
  where
    unpack ref = fromJust $ deref e ref

getTrace :: (ToJSON c, FromJSON c) => Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
getTrace e = getPathFromTo e initialRef (currRef e)

getTraces :: (ToJSON c, FromJSON c) => Explorer p m c o -> [[((Ref, c), (p, o), (Ref, c))]]
getTraces e = getPathsFromTo e initialRef (currRef e)

mapOut :: (ToJSON c, FromJSON c) => Explorer p m c o -> Gr Ref (p, o) -> [Ref] -> Ref -> (Ref, Ref, (p,o)) -> Maybe [[((Ref, c), (p, o), (Ref, c))]]
mapOut exp gr visited goal (s, t, (l, o))
  | goal == t = Just $ [((s, unpack s), (l, o), (t, unpack t))] : explore
  | otherwise = if t `elem` visited then Nothing else Just explore
  where
    explore = map (((s, unpack s), (l, o), (t, unpack t)) :) (concat $ mapMaybe (mapOut exp gr (t : visited) goal) (out gr t))
    unpack ref = fromJust $ deref exp ref

getPathsFromTo :: (ToJSON c, FromJSON c) => Explorer p m c o -> Ref -> Ref -> [[((Ref, c), (p, o), (Ref, c))]]
getPathsFromTo exp from to = concat $ mapMaybe (mapOut exp (execEnv exp) [from] to) (out (execEnv exp) from)

getPathFromTo :: (ToJSON c, FromJSON c) => Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo exp from to =
  case getPathsFromTo exp from to of
    [] -> []
    (x:_) -> x

executionGraph :: (ToJSON c, FromJSON c) => Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph exp =
  (curr, nodes, edges)
  where
    curr = (currRef exp, config exp)
    nodes = map (second (fromJust . getConfig exp)) $ IntMap.toList $ cmap exp
    edges = map (\(s, t, p) -> ((s, fromJust $ deref exp s), p, (t, fromJust $ deref exp t)) ) (labEdges (execEnv exp))

leaves :: (ToJSON c, FromJSON c) => Explorer p m c o -> [(Ref, c)]
leaves exp = map refToPair leaf_nodes
  where
    env = execEnv exp
    refToPair = \r -> (r, fromJust $ deref exp r)
    leaf_nodes = nodes $ nfilter ((==0) . outdeg env) env

toExport :: Explorer p m c o -> (Ref, [(Ref, ConfigDiff c)], [(Ref, Ref, (p, o))])
toExport exp = (currRef exp, IntMap.toList $ cmap exp, labEdges $ execEnv exp)

fromExport :: Explorer p m c o -> (Ref, [(Ref, ConfigDiff c)], [(Ref, Ref, (p, o))]) -> Explorer p m c o
fromExport exp (curr, nds, edgs) = exp { genRef = findMax nds,
                                         config = findCurrentConf curr nds,
                                         currRef = curr,
                                         cmap = IntMap.fromList nds,
                                         execEnv = mkGraph (map (\(x, _) -> (x, x)) nds) edgs }
  where findMax l = maximum $ map fst l
        findCurrentConf curr nds = case lookup curr nds of
                                     Just (FullConfig conf) -> conf
                                     _ -> error "no config found"
