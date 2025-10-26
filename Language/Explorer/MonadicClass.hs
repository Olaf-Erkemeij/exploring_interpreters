{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Explorer.MonadicClass
  ( Explorer,
    mkExplorer
  )
where

import Language.Explorer.Class
    ( ExplorerM(..), Language, Ref, initialRef )

import Control.DeepSeq (NFData)
import Data.Foldable (find, foldlM)
import Data.Graph.Inductive.Graph
    ( insEdge,
      insNode,
      nfilter,
      nodes,
      out,
      outdeg,
      suc,
      Graph(labEdges, mkGraph),
      Node )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query (reachable)
import qualified Data.IntMap.Strict as IntMap
import Data.List ((\\))
import Data.Maybe (fromJust, mapMaybe)
import Data.Tree (Tree (..))

data Explorer programs m configs output where
  Explorer ::
    (Language programs m configs output, NFData programs, NFData configs, NFData output) =>
    { defInterp :: programs -> configs -> m (Maybe configs, output),
      _config :: !configs,
      _currRef :: !Ref,
      _genRef :: !Ref,
      _cmap :: !(IntMap.IntMap configs),
      _execEnv :: !(Gr Ref (programs, output))
    } ->
    Explorer programs m configs output

mkExplorer :: (Language p m c o, NFData p, NFData c, NFData o) => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer definterp conf =
  Explorer
    { defInterp = definterp,
      _config = conf,
      _genRef = 1,
      _currRef = initialRef,
      _cmap = IntMap.fromList [(initialRef, conf)],
      _execEnv = mkGraph [(initialRef, initialRef)] []
    }

deref' :: Explorer p m c o -> Ref -> Maybe c
deref' e r = IntMap.lookup r (_cmap e)

updateExecEnvs' :: Explorer p m c o -> (p, c, o) -> Explorer p m c o
updateExecEnvs' e (p, newconf, output) = e
    { _config = newconf,
      _currRef = newref,
      _genRef = newref,
      _cmap = IntMap.insert newref newconf (_cmap e),
      _execEnv = insNode (newref, newref) $ insEdge (_currRef e, newref, (p, output)) (_execEnv e)
    }
  where
    newref = _genRef e + 1

execute' :: Monad m => p -> Explorer p m c o -> m (Explorer p m c o, o)
execute' p e = do
    (mcfg, o) <- defInterp e p (_config e)
    case mcfg of
      Just cfg -> return (updateExecEnvs' e (p, cfg, o), o)
      Nothing -> return (e, o)

data RevertableStatus = ContinueRevert | StopRevert deriving (Show)

findRevertableNodes :: Gr a b -> Node -> Node -> [Node]
findRevertableNodes gr source target = case find (\n -> target `elem` reachable n gr) (suc gr source) of
    Just node -> fst $ findRevertableNodes' gr node target
    Nothing   -> []
  where
    findRevertableNodes' gr' current end
      | current == end = if outdeg gr' current > 1 then ([], StopRevert) else ([current], ContinueRevert)
      | otherwise = case find (\n -> end `elem` reachable n gr') (suc gr' current) of
          Just node -> case findRevertableNodes' gr' node end of
            (res, StopRevert)     -> (res, StopRevert)
            (res, ContinueRevert) -> if outdeg gr' current > 1 then (res, StopRevert) else (current : res, ContinueRevert)
          Nothing -> ([], ContinueRevert)

jump' :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump' r e = case deref' e r of
  Just c -> Just $ e {_config = c, _currRef = r}
  Nothing -> Nothing

revert' :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert' r e
  | _currRef e `elem` reachNodes =
      jump' r e >>= \e' ->
        return $
          e'
            { _execEnv = mkGraph (zip remainNodes remainNodes) $ filter (\(_, t, _) -> t `notElem` reachNodes) (labEdges $ _execEnv e'),
              _cmap = foldl (flip IntMap.delete) (_cmap e') reachNodes
            }
  | otherwise = Nothing
  where
    reachNodes = findRevertableNodes gr r (_currRef e)
    remainNodes = nodes gr \\ reachNodes
    gr = _execEnv e

toTree' :: Explorer p m c o -> Tree (Ref, c)
toTree' exp = mkTree initialRef
  where
    graph = _execEnv exp
    target (_, r, _) = r
    mkTree r = Node (r, _cmap exp IntMap.! r) (map (mkTree . target) (out graph r))

leaves' :: Explorer p m c o -> [(Ref, c)]
leaves' exp = map (\r -> (r, fromJust $ deref' exp r)) leaf_nodes
  where
    env = _execEnv exp
    leaf_nodes = nodes $ nfilter ((== 0) . outdeg env) env

getPathFromTo' :: Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
getPathFromTo' exp from to = case getPathsFromTo' exp from to of
    []      -> []
    (x : _) -> x

getPathsFromTo' :: Explorer p m c o -> Ref -> Ref -> [[((Ref, c), (p, o), (Ref, c))]]
getPathsFromTo' exp from to = concat $ mapMaybe (mapOut exp (_execEnv exp) [from] to) (out (_execEnv exp) from)
  where
    unpack ref = fromJust $ deref' exp ref
    mapOut e gr visited goal (s, t, (l, o))
      | goal == t = Just $ [((s, unpack s), (l, o), (t, unpack t))] : explore
      | t `elem` visited = Nothing
      | otherwise = Just explore
      where
        explore = map (((s, unpack s), (l, o), (t, unpack t)) :) (concat $ mapMaybe (mapOut e gr (t : visited) goal) (out gr t))

-- INSTANCE DECLARATION
instance (NFData p, NFData c, NFData o, Language p m c o) => ExplorerM (Explorer p m c o) m p c o where
    execute p e = execute' p e

    executeAll ps e = foldlM (\(e', out) p' -> do (res, out') <- execute' p' e'; return (res, out `mappend` out')) (e, mempty) ps

    jump r e = return $ jump' r e

    revert r e = return $ revert' r e

    deref r e = return $ deref' e r

    config e = return $ _config e

    currRef e = return $ _currRef e

    toTree e = return $ toTree' e

    leaves e = return $ leaves' e

    getTrace e = return $ getPathFromTo' e initialRef (_currRef e)