{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}


module Language.Explorer.Algebraic where

--     ( Explorer
--     , mkExplorer
--     , mkExplorerNoSharing
--     , execute
--     , executeAll
--     , revert
--     , jump
--     , toTree
--     , incomingEdges
--     , config
--     , execEnv
--     , currRef
--     , leaves
--     , cmap
--     , Ref
--     , Language
--     , deref
--     , getTrace
--     , getTraces
--     , getPathsFromTo
--     , getPathFromTo
--     , executionGraph
--     , initialRef
--     , fromExport
--     , toExport
--     ) where

-- import qualified Algebra.Graph.Labelled as L
-- import Algebra.Graph.Labelled (Graph, vertex, edge)
-- import Data.Tree (Tree(..))

-- import qualified Data.IntMap as IntMap
-- import Data.List
-- import Data.Foldable
-- import Data.Maybe

-- type Ref = Int
-- type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

-- data Explorer programs m configs output where
--     Explorer :: Language programs m configs output =>
--         {defInterp :: programs -> configs -> m (Maybe configs, output)
--         , config :: configs
--         , currRef :: Ref
--         , genRef :: Ref
--         , cmap :: IntMap.IntMap configs
--         , execEnv :: Graph (programs, output) Ref
--         , configEq :: configs -> configs -> Bool
--         } -> Explorer programs m configs output


-- mkExplorer :: Language p m c o
--            => Bool
--            -> (c -> c -> Bool)
--            -> (p -> c -> m (Maybe c, o))
--            -> c
--            -> Explorer p m c o
-- mkExplorer shadow shadowEq definterp conf = Explorer
--     { defInterp = definterp
--     , config = conf
--     , genRef = 1 -- Currently generate references by increasing a counter.
--     , currRef = initialRef
--     , cmap = IntMap.fromList [(initialRef, conf)]
--     , execEnv = L.vertex initialRef
--     , configEq = shadowEq
-- }

-- initialRef :: Int
-- initialRef = 1

-- mkExplorerNoSharing :: Language p m c o  => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
-- mkExplorerNoSharing = mkExplorer False (\_ _ -> False)

-- deref :: Explorer p m c o -> Ref -> Maybe c
-- deref e r = IntMap.lookup r (cmap e)

-- findRef :: Explorer p m c o -> c -> (c -> Bool) -> Maybe (Ref, c)
-- findRef e c eq = find (\(r, c') -> eq c') (IntMap.toList (cmap e))

-- addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
-- addNewPath e p o c = e
--     { config = c
--     , currRef = newref
--     , genRef = newref
--     , cmap = IntMap.insert newref c (cmap e)
--     , execEnv = L.edge (p, o) (currRef e) newref `L.overlay` execEnv e
--     }
--   where newref = genRef e + 1
--         newVertex = L.vertex newref

-- updateExecEnvs :: Language p m c o => Explorer p m c o -> (p, c, o) -> Explorer p m c o
-- updateExecEnvs e (p, newconf, output) = addNewPath e p output newconf

-- execute :: Language p m c o =>  p -> Explorer p m c o -> m (Explorer p m c o, o)
-- execute p e =
--   do (mcfg, o) <- defInterp e p (config e)
--      case mcfg of
--        Just cfg -> return (updateExecEnvs e (p, cfg, o), o)
--        Nothing  -> return (e, o)

-- executeAll :: Language p m c o => [p] -> Explorer p m c o -> m (Explorer p m c o, o)
-- executeAll ps exp = foldlM executeCollect (exp, mempty) ps
--   where executeCollect (exp, out) p = do (res, out') <- execute p exp
--                                          return (res, out `mappend` out')

-- deleteMap :: [Ref] -> IntMap.IntMap a -> IntMap.IntMap a
-- deleteMap xs m = foldl (flip IntMap.delete) m xs

-- cleanEdges :: [Ref] -> [(Ref, Ref, (p, o))] -> [(Ref, Ref, (p, o))]
-- cleanEdges ref = filter (\(s, t, l) -> t `notElem` ref)

-- data RevertableStatus = ContinueRevert | StopRevert deriving Show

-- suc :: Ref -> L.Graph (p, o) Ref -> [Ref]
-- suc n graph = [t | (_, s, t) <- L.edgeList graph, s == n]

-- reachable :: Ref -> L.Graph (p, o) Ref -> [Ref]
-- reachable start graph = bfs [start] []
--   where
--     bfs [] visited = reverse visited
--     bfs (n:ns) visited
--         | n `elem` visited = bfs ns visited
--         | otherwise =
--             let successors = suc n graph
--             in bfs (ns ++ successors) (n:visited)

-- outdeg :: Ref -> L.Graph (p, o) Ref -> Int
-- outdeg n = length . suc n

-- out g n = filter (\(s, _, _) -> s == n) (L.edgeList g)

-- findRevertableNodes gr source target =
--   case findNextNodeInPath gr source target of
--     (Just node) -> fst $ findRevertableNodes' gr node target
--     Nothing     -> []
--   where
--     findNextNodeInPath g s t = listToMaybe [n | n <- suc s g, t `elem` reachable n g]
--     findRevertableNodes' g s t
--       | s == t = if outdeg s g > 1 then ([], StopRevert) else ([source], ContinueRevert)
--       | otherwise = case findNextNodeInPath gr source target of
--         (Just node) -> case findRevertableNodes' gr node target of
--           (res, StopRevert) -> (res, StopRevert)
--           (res, ContinueRevert) -> if outdeg s g > 1 then (res, StopRevert) else (source : res, ContinueRevert)
--         Nothing -> ([], ContinueRevert)

-- revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
-- revert r e
--   | currRef e `elem` reachNodes = do
--     e' <- jump r e
--     let cleanedEdges = filter (\(s, t, _) -> t `notElem` reachNodes) $ L.edgeList (execEnv e')
--     return $ e' { execEnv = L.overlay (L.edges cleanedEdges) (execEnv e')
--                 , cmap = deleteMap remainNodes (cmap e')
--                 }
--   | otherwise = Nothing
--   where
--     reachNodes = findRevertableNodes gr r (currRef e)
--     remainNodes = L.vertexList gr \\ reachNodes
--     gr = execEnv e

-- jump :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
-- jump r e = case deref e r of
--              (Just c) -> return $ e { config = c, currRef = r }
--              Nothing -> Nothing

-- toTree :: Explorer p m c o -> Tree (Ref, c)
-- toTree exp = mkTree initialRef
--   where graph = execEnv exp
--         target (_, r, _) = r
--         mkTree r = Node (r, cmap exp IntMap.! r) (map (mkTree . target) (L.edgeList graph))


-- incomingEdges :: Ref -> Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
-- incomingEdges ref e = foldr (\(l, s, t) acc ->  ((s, unpack s), l, (t, unpack t)) : acc) [] (filter (\(_, t, _) -> t == ref) (L.edgeList (execEnv e)))
--   where
--     unpack ref = fromJust $ deref e ref

-- getTrace :: Explorer p m c o -> [((Ref, c), (p, o), (Ref, c))]
-- getTrace e = getPathFromTo e initialRef (currRef e)

-- getTraces :: Explorer p m c o -> [[((Ref, c), (p, o), (Ref, c))]]
-- getTraces e = getPathsFromTo e initialRef (currRef e)


-- mapOut :: Explorer p m c o
--        -> L.Graph (p, o) Ref
--        -> [Ref]
--        -> Ref
--        -> ((p, o), Ref, Ref)
--        -> Maybe [[((p, o), (Ref, c), (Ref, c))]]
-- mapOut exp gr visited goal ((l, o), s, t)
--   | goal == t = Just [[((l, o), (s, unpack s), (t, unpack t))]]
--   | t `elem` visited = Nothing
--   | otherwise =
--       let -- Get all outgoing edges from current target node
--           outgoing = filter (\(_, s', _) -> s' == t) (L.edgeList gr)
--           -- Recursively explore each outgoing edge
--           subpaths = concatMap (fromMaybe [] . mapOut exp gr (t:visited) goal) outgoing
--           -- Prepend current edge to all found paths
--           paths = map (((l, o), (s, unpack s), (t, unpack t)) :) subpaths
--       in if null paths then Nothing else Just paths
--   where
--     unpack ref = fromJust $ deref exp ref


-- getPathsFromTo :: Explorer p m c o -> Ref -> Ref -> [[((p, o), (Ref, c), (Ref, c))]]
-- getPathsFromTo exp from to =
--   concat $ mapMaybe (mapOut exp (execEnv exp) [from] to)
--     (filter (\(_, s, _) -> s == from) (L.edgeList (execEnv exp)))

-- getPathFromTo :: Explorer p m c o -> Ref -> Ref -> [((Ref, c), (p, o), (Ref, c))]
-- getPathFromTo exp from to =
--   case getPathsFromTo exp from to of
--     [] -> []
--     (x:_) -> x

-- executionGraph :: Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
-- executionGraph exp =
--   (curr, nodes, edges)
--   where
--     curr = (currRef exp, config exp)
--     nodes = IntMap.toList $ cmap exp
--     edges = map (\(p, s, t) -> ((s, fromJust $ deref exp s), p, (t, fromJust $ deref exp t)) ) (L.edgeList (execEnv exp))

-- isLeaf :: Ref -> L.Graph (p, o) Ref -> Bool
-- isLeaf n g = null [t | (_, s, t) <- L.edgeList g, s == n]

-- {-|
--   Returns all configurations that have not been the source for an execute action.
--   This corresponds to leaves in a tree or nodes without an outbound-edge in a graph.
-- -}
-- leaves :: Explorer p m c o -> [(Ref, c)]
-- leaves exp =
--   let allNodes = IntMap.keys (cmap exp)
--       graph = execEnv exp
--   in [(r, fromJust $ deref exp r) | r <- allNodes, isLeaf r graph]


-- toExport :: Explorer p m c o -> (Ref, [(Ref, c)], [((p, o), Ref, Ref)])
-- toExport exp = (currRef exp, IntMap.toList $ cmap exp, L.edgeList $ execEnv exp)

-- fromExport :: Explorer p m c o -> (Ref, [(Ref, c)], [((p, o), Ref, Ref)]) -> Explorer p m c o
-- fromExport exp (curr, nds, edgs) = exp { genRef = findMax nds,
--                                          config = findCurrentConf curr nds,
--                                          currRef = curr,
--                                          cmap = IntMap.fromList nds,
--                                          execEnv = L.overlay (L.vertices vs) (L.edges edgs) }
--   where findMax l = maximum $ map fst l
--         findCurrentConf curr nds = case lookup curr nds of
--                                      Just conf -> conf
--                                      Nothing   -> error "no config found"
--         vs = map fst nds

