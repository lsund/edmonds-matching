module Algorithm.Edmonds.General.Core (findRoot) where

import Protolude
import Util
import Data.Graph.Core as Graph
import Algorithm.Edmonds.General.Helpers
import Data.Maybe

import qualified Data.AlternatingForest as AF
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import qualified Data.List as List

--------------------------------------------------------------------------------
-- Description
--
-- Implementation of Edmonds Cardinality Matching. It works by passing
-- control between the functions findRoot, findNeighbour, grow,
-- augment and shrink, and updating a state in form of a special graph.
--
-- The 5 functions directly corresponding to the
-- lines 2-6 of the algorithm described on pages 224-225 in Korte,
-- Vygen 2002.

--------------------------------------------------------------------------------
-- Implementation
--
-- Finds a vertex to start growing the alternating forest from. If no
-- suitable vertex is found, then stop and return the graph of the
-- current state. Else find a neighbour for this vertex.
findRoot :: Graph -> Graph
findRoot graph =
  let mx = find (\x -> x `Set.notMember` scanned graph && isOuter graph x) vs
  in case mx of
       Nothing -> graph
       Just x -> findNeighbour $ graph {currentX = x}
  where
    vs = vertices graph


-- Attempts to find a neighbour to create a new edge used to grow the alternating
-- forest. If successful, grow the edge. Else, find a new root vertex.
findNeighbour :: Graph -> Graph
findNeighbour graph =
  let match y =
        ( isOutOfForest graph y
        || (isOuter graph y && (not . sameBlossom graph x) y))
      nbs = neighbours graph x
      found = find match nbs
  in case found of
       Nothing ->
         let scanned' = Set.insert x $ scanned graph
         in findRoot (graph {scanned = scanned'})
       Just y -> grow (graph {currentY = y})
  where
    x = currentX graph


-- Attempt to use the edge found to grow the alternating forest. If successful,
-- augment the root path from this edge. Else, find another neighbour.
grow :: Graph -> Graph
grow graph =
  let (x, y) = (currentX graph, currentY graph)
      phi = (AF.phi . forest) graph
  in if isOutOfForest graph y
       then let forest' = (forest graph) {AF.phi = Map.insert y x phi}
            in findNeighbour (graph {forest = forest'})
       else if isBipartite graph
            then bipartiteAugment graph
            else augment graph

-- Augment the path from the last edge grown to the root, assuming a bipartite
-- graph and no odd cycles. Then find a new root.
bipartiteAugment :: Graph -> Graph
bipartiteAugment graph =
  let (x, y) = (currentX graph, currentY graph)
      (px, py) = (pathToRoot graph X, pathToRoot graph Y)
      mu = (AF.mu . forest) graph
      phi = (AF.phi . forest) graph
  in
    let (oddpx, oddpy) = odds px py
        u = oddpx `Set.union` oddpy
        pu = Set.foldr (\x acc -> (x, Graph.get phi x) : acc) [] u
        mu' = insertListSymmetric ((x, y) : (y, x) : pu) mu
        graph' = resetForest graph mu'
     in findRoot graph'

-- Attempt to augment the path from the last edge grown to the root.
-- If an odd cycle is found on the root-path, shrink this cycle. Else
-- augment the path and find a new root.
augment :: Graph -> Graph
augment graph =
  let (x, y) = (currentX graph, currentY graph)
      (px, py) = (pathToRoot graph X, pathToRoot graph Y)
      mu = (AF.mu . forest) graph
      phi = (AF.phi . forest) graph
      isect = Set.fromList px `Set.intersection` Set.fromList py
  in if Set.null isect
       then let (oddpx, oddpy) = odds px py
                u = oddpx `Set.union` oddpy
                pu = Set.foldr (\x acc -> (x, Graph.get phi x) : acc) [] u
                mu' = insertListSymmetric ((x, y) : (y, x) : pu) mu
                graph' = resetForest graph mu'
            in findRoot graph'
       else shrink graph

-- Shrink the odd cycle on the root-path from the current edge. Then find a new
-- neighbour.
shrink :: Graph -> Graph
shrink graph =
  let (x, y, vs)           = (currentX graph, currentY graph, vertices graph)
      (r, union, filtered) = getBlossom graph
      (phi, ro)            = ((AF.phi . forest) graph, (AF.ro . forest) graph)
      adjustPhi keys m     =
        Set.foldr (\k acc -> Map.insert k (Graph.get phi k) acc) m keys
      phi'                 = adjustPhi filtered phi
      phi''                = symmetricUpdate (Graph.get ro) r x y phi'
      keys'                = filter (\x -> Graph.get ro x `Set.member` union) vs
      ro'                  = insertList (zip keys' (repeat r)) ro
      forest'              = (forest graph) {AF.phi = phi'', AF.ro = ro'}
  in findNeighbour $ graph {forest = forest'}
