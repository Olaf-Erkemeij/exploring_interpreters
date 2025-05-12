{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Explorer.Zipper
    ( Explorer
    , mkExplorer
    , mkExplorerNoSharing
    , execute
    , executeAll
    , revert
    , jump
    , config
    , currRef
    , leaves
    , treePos
    , Ref
    , Language
    , deref
    , getTrace
    , executionGraph
    , initialRef
    , fromExport
    , toExport
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Query.SP
import Data.Tree (Tree(..))

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Tree.Zipper as Z
import Data.List
import Data.Foldable
import Data.Maybe

import qualified Data.Hashable as H
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Function (on)

type Ref = Int
type Language p m c o = (Eq p, Eq o, Monad m, Monoid o)

data NodeData p c o = NodeData
    { nodeRef     :: !Ref
    , nodeConfig  :: !c
    , nodeProgram :: !(Maybe (p, o))
    }

data Explorer programs m configs output where
    Explorer :: Language programs m configs output =>
        { defInterp :: programs -> configs -> m (Maybe configs, output)
        , treePos :: !(Z.TreePos Z.Full (NodeData programs configs output))
        , genRef :: !Ref
        } -> Explorer programs m configs output

mkExplorer :: Language p m c o => Bool -> (c -> c -> Bool) -> (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorer shadow shadowEq definterp conf = Explorer
    { defInterp = definterp
    , treePos = Z.fromTree $ Node (NodeData initialRef conf Nothing) []
    , genRef = initialRef + 1
}

initialRef :: Int
initialRef = 1

mkExplorerNoSharing :: Language p m c o  => (p -> c -> m (Maybe c, o)) -> c -> Explorer p m c o
mkExplorerNoSharing = mkExplorer False (\_ _ -> False)

config :: Explorer p m c o -> c
config e = nodeConfig $ Z.label (treePos e)

currRef :: Explorer p m c o -> Ref
currRef e = nodeRef $ Z.label (treePos e)

deref :: Explorer p m c o -> Ref -> Maybe c
deref e r = fmap (nodeConfig . Z.label) (findNode r (Z.root (treePos e)))

findExisting :: Explorer p m c o -> c -> Tree (NodeData p c o) -> Maybe Ref
findExisting e conf (Node nd cs) = msum $ map (findExisting e conf) cs

moveToRef :: Ref -> Z.TreePos Z.Full (NodeData p c o) -> Z.TreePos Z.Full (NodeData p c o)
moveToRef target pos = fromMaybe pos (findNode target pos)

appendChild :: a -> Z.TreePos Z.Full a -> Z.TreePos Z.Full a
appendChild child pos =
  let updatedPos = Z.modifyTree (\(Node lab cs) -> Node lab (cs ++ [Node child []])) pos
      tree = Z.toTree updatedPos
  in case tree of
       Node _ cs ->
         if null cs
            then updatedPos
            else fromMaybe (Z.fromTree (last cs)) (descendPath [length cs - 1] updatedPos)

addNewPath :: Explorer p m c o -> p -> o -> c -> Explorer p m c o
addNewPath e p o c =
    case findExisting e c (Z.toTree (treePos e)) of
        Just existingRef ->
            e { treePos = moveToRef existingRef (treePos e) }
        Nothing ->
            let newRef = genRef e
                newData = NodeData newRef c (Just (p, o))
                newPos = appendChild newData (treePos e)
            in e { treePos = newPos, genRef = newRef + 1 }

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

findPath :: Ref -> Tree (NodeData p c o) -> Maybe [Int]
findPath target (Node nd cs)
  | nodeRef nd == target = Just []
  | otherwise = searchChildren 0 cs
  where
    searchChildren _ [] = Nothing
    searchChildren i (t:ts) =
      case findPath target t of
        Just path -> Just (i : path)
        Nothing   -> searchChildren (i + 1) ts

descendPath :: [Int] -> Z.TreePos Z.Full a -> Maybe (Z.TreePos Z.Full a)
descendPath [] pos = Just pos
descendPath (i:is) pos =
  let Node _ cs = Z.toTree pos
  in if i < 0 || i >= length cs
        then Nothing
        else let childTree = cs !! i
                 childZipper = Z.fromTree childTree
             in descendPath is childZipper

findNode :: Ref -> Z.TreePos Z.Full (NodeData p c o) -> Maybe (Z.TreePos Z.Full (NodeData p c o))
findNode target pos = do
  path <- findPath target (Z.toTree pos)
  descendPath path pos

revert :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
revert target e =
    case findNode target (Z.root (treePos e)) of
        Just zipper ->
            let cleared = Z.modifyTree (\t -> t { subForest = [] }) zipper
                newPos  = fromMaybe cleared (Z.parent cleared)
            in Just $ e { treePos = newPos }
        Nothing -> Nothing

jump :: Ref -> Explorer p m c o -> Maybe (Explorer p m c o)
jump r e = case findNode r (Z.root (treePos e)) of
    Just zipper -> Just $ e { treePos = zipper }
    Nothing -> Nothing

getTrace :: Explorer p m c o -> [((Ref, c), Maybe (p, o))]
getTrace e = reverse $ go (treePos e) []
  where
    go pos acc =
        case Z.parent pos of
            Just p -> go p (((nodeRef (Z.label p), nodeConfig (Z.label p)), nodeProgram (Z.label pos)) : acc)
            Nothing -> acc

mapOut :: Explorer p m c o -> Gr Ref (p, o) -> [Ref] -> Ref -> (Ref, Ref, (p,o)) -> Maybe [[((Ref, c), (p, o), (Ref, c))]]
mapOut exp gr visited goal (s, t, (l, o))
  | goal == t = Just $ [((s, unpack s), (l, o), (t, unpack t))] : explore
  | otherwise = if t `elem` visited then Nothing else Just explore
  where
    explore = map (((s, unpack s), (l, o), (t, unpack t)) :) (concat $ mapMaybe (mapOut exp gr (t : visited) goal) (out gr t))
    unpack ref = fromJust $ deref exp ref

executionGraph :: Explorer p m c o -> ((Ref, c), [(Ref, c)], [((Ref, c), (p, o), (Ref, c))])
executionGraph e =
    let rootPos = Z.root (treePos e)
        tree = Z.toTree rootPos
        nodes = collectNodes tree
        edges = collectEdges' tree
    in ((currRef e, config e), nodes, edges)

leaves :: Explorer p m c o -> [(Ref, c)]
leaves e =
    let rootPos = Z.root (treePos e)
        tree = Z.toTree rootPos
    in filterLeaf tree
  where
    filterLeaf (Node nd []) = [(nodeRef nd, nodeConfig nd)]
    filterLeaf (Node _ cs) = concatMap filterLeaf cs

collectNodes :: Tree (NodeData p c o) -> [(Ref, c)]
collectNodes (Node nd subtrees) = (nodeRef nd, nodeConfig nd) : concatMap collectNodes subtrees

nodeEdges :: Foldable t => NodeData p c1 o -> t (Z.TreePos Z.Full (NodeData a c2 b)) -> [(Ref, Ref, (a, b))]
nodeEdges l = concatMap (\c -> case nodeProgram (Z.label c) of
                                Just (p, o) -> [(nodeRef l, nodeRef (Z.label c), (p, o))]
                                Nothing -> []
                        )
collectEdges' :: Tree (NodeData p c o) -> [((Ref, c), (p, o), (Ref, c))]
collectEdges' (Node nd cs) =
    concatMap (\t@(Node childNd _) ->
                 case nodeProgram childNd of
                   Just io -> ((nodeRef nd, nodeConfig nd), io, (nodeRef childNd, nodeConfig childNd))
                              : collectEdges' t
                   Nothing -> collectEdges' t
              ) cs

collectEdges :: Tree (NodeData p c o) -> [(Ref, Ref, (p, o))]
collectEdges (Node nd subtrees) =
    concatMap (\t@(Node childNd _) ->
                 case nodeProgram childNd of
                   Just io -> (nodeRef nd, nodeRef childNd, io)
                              : collectEdges t
                   Nothing -> collectEdges t
              ) subtrees

toExport :: Explorer p m c o -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))])
toExport exp =
    let tree = Z.toTree (treePos exp)
        nodes = collectNodes tree
        edges = collectEdges tree
    in (currRef exp, nodes, edges)

rebuildTree :: forall c p o. [(Ref, c)] -> [(Ref, Ref, (p, o))] -> Tree (NodeData p c o)
rebuildTree nodes edges =
    let nodeMap :: M.Map Ref c
        nodeMap = M.fromList nodes

        progMap :: M.Map Ref (p, o)
        progMap = M.fromList [ (child, io) | (_, child, io) <- edges ]

        childrenMap :: M.Map Ref [Ref]
        childrenMap = M.fromListWith (++) [ (parent, [child]) | (parent, child, _) <- edges ]

        buildTree :: Ref -> Tree (NodeData p c o)
        buildTree ref =
            let conf = fromMaybe (error "Missing node during import") (M.lookup ref nodeMap)
                prog = if ref == initialRef then Nothing else M.lookup ref progMap
                childRefs = M.findWithDefault [] ref childrenMap
                childrenTrees = map buildTree childRefs
            in Node (NodeData ref conf prog) childrenTrees

    in buildTree initialRef

fromExport :: Language p m c o
           => Explorer p m c o
           -> (Ref, [(Ref, c)], [(Ref, Ref, (p, o))])
           -> Explorer p m c o
fromExport exp (curr, nodes, edges) =
    let tree = rebuildTree nodes edges
        zipper = fromMaybe (error "Current ref not found in imported tree")
                           (findNode curr (Z.fromTree tree))
        newGen = maximum (map fst nodes) + 1
    in exp { treePos = zipper, genRef = newGen }