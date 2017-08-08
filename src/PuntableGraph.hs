{-# LANGUAGE TypeSynonymInstances #-}

module PuntableGraph
(
  PuntableGraph (),
  EdgeType (ClaimedEdge, OptionEdge, OpenEdge),
  buildPuntableGraph,
  getConnectedSites,
  findBridges,
  findMinimumPathLength,
  findAllMinimumPathLengths,
  splitOnBridge,
  splitOnCut,
  findMinimumCut
)
  where

import BaseTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.MaxFlow
import Data.List (groupBy, concatMap)
import Data.Tree
import Data.Tuple

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.Set as S

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Loops

data EdgeType = ClaimedEdge | OptionEdge | OpenEdge deriving (Show, Read, Eq);

data PuntableGraph = PuntableGraph {
  graph :: Gr () EdgeType
}

edgeTypeToWeight :: EdgeType -> Int
edgeTypeToWeight OpenEdge = 1
edgeTypeToWeight ClaimedEdge = 1024
edgeTypeToWeight OptionEdge = 1024

buildPuntableGraph :: [Site] -> [(River, EdgeType)] -> PuntableGraph
buildPuntableGraph sites typedRivers = PuntableGraph graph
  where
    graph = undir (mkGraph labUNodes labEdges)
    labUNodes = map (flip (,) ()) sites
    labEdges = map (\(river, edgeType) -> let (src, tgt) = riverSites river in (src, tgt, edgeType)) typedRivers

getConnectedSites :: PuntableGraph -> Site -> [Site]
getConnectedSites pg source = reachable source (graph pg)

treePairs :: Tree a -> [(a, a)]
treePairs (Node k children) = foldl (pairs k) [] children
  where
    pairs :: a -> [(a, a)] -> Tree a -> [(a, a)]
    pairs parent priors (Node k children) = (parent, k):(foldl (pairs k) priors children)

chainDecomposition :: (Gr a b) -> Node -> [[Edge]]
chainDecomposition g root = if null outbound_trees then [] else (filter (not . null) resultChains)
  where
    outbound_trees = dff [root] g
    outbound_tree = head outbound_trees
    forward_pairs = treePairs outbound_tree
    reverse_pairs = map swap forward_pairs
    reverse_lookup = M.fromList reverse_pairs
    edge_in_tree_lookup = S.fromList (forward_pairs ++ reverse_pairs)
    -- Find edges not in this tree
    backlinks :: [Edge]
    backlinks = filter (\(a,b) -> (not (S.member (a, b) edge_in_tree_lookup)) && (not (S.member (b, a) edge_in_tree_lookup))) (edges g)
    chainSearch :: State (S.Set Node, [[Edge]], Int) ()
    chainSearch  = mapM_ (\node -> do
      -- TODO: Traverse back links first
      let node_backlinks = filter (\(a,b) -> a == node) backlinks
      forM node_backlinks (\(src, tgt) -> do
        (labelled, chains, cnode) <- get
        put (S.insert src labelled, [(src, tgt)]:chains, tgt)
        whileM (get >>= (\(labelled, chains, cnode) -> return ((not (cnode == root)) && (S.notMember cnode labelled)))) (do
            (labelled, chains, cnode) <- get
            let next_node = (M.!) reverse_lookup cnode
            put (S.insert cnode labelled, ((cnode, next_node):(head chains)):(tail chains), next_node)
          )
        )
      ) (preorder outbound_tree)
    (_, (_, resultChains, _)) = runState chainSearch (S.empty, [], 0)

-- Finds bridges via chain decomposition.
-- See README Reference [1]
findBridges :: PuntableGraph -> [(Site, Site)]
findBridges pg = concatMap findSectionBridges sections
  where
    sections = components (graph pg)
    findSectionBridges :: [Node] -> [Edge]
    findSectionBridges [] = []
    findSectionBridges section@(root:_) = bridges
      where
        cdecomp = chainDecomposition (graph pg) root
        usedEdges = S.fromList (concat cdecomp)
        sectionNodes = S.fromList section
        sectionEdges = filter (\(a, b) -> S.member a sectionNodes) (filter (\(a, b) -> b > a) (edges (graph pg)))
        bridges = filter (\(a, b) -> (S.notMember (a, b) usedEdges) && (S.notMember (b, a) usedEdges)) sectionEdges

splitOnBridge :: PuntableGraph -> (Site, Site) -> ([Site], [Site])
splitOnBridge (PuntableGraph g) bridge@(a, b) = (aside, bside)
  where
    withoutBridge = delEdge bridge g
    aside = reachable a withoutBridge
    bside = reachable b withoutBridge

splitOnCut :: PuntableGraph -> [(Site, Site)] -> ([Site], [Site])
splitOnCut (PuntableGraph g) bridges = if null bridges then ([], []) else (aside, bside)
  where
    (a,b) = head bridges
    withoutBridge = delEdges bridges g
    aside = reachable a withoutBridge
    bside = reachable b withoutBridge

findMinimumPathLength :: PuntableGraph -> Site -> Site -> Maybe Int
findMinimumPathLength pg source dest = Just (length (esp source dest (graph pg)))

findAllMinimumPathLengths :: PuntableGraph -> Site -> [(Site, Int)]
findAllMinimumPathLengths pg source = level source (graph pg)

findMinimumCut :: PuntableGraph -> Site -> Site -> Maybe [Edge]
findMinimumCut pg src tgt =
  if isConnected
     then Just maxedEdges
     else Nothing
  where
    isConnected = any ((==) tgt) (getConnectedSites pg src)
    weightedGraph = emap edgeTypeToWeight (graph pg)
    mfg = maxFlowgraph weightedGraph src tgt
    maxedEdges = edges (efilter (\(a,b,(cflow,lflow)) -> cflow >= lflow) mfg)
