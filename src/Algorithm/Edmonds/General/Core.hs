module Algorithm.Edmonds.General.Core (findRoot, findNeighbour)
where

import Algorithm.Edmonds.General.Helpers
import qualified Data.AlternatingForest as AF
import Data.Graph.Core as Graph
import Util

import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import qualified Data.List as List
import Data.Maybe
import Protolude

findRoot :: Graph -> Graph
findRoot graph =
  let mx = find (\x -> x `Set.notMember` scanned graph && isOuter graph x) vs
  in case mx of
       Nothing -> graph
       Just x -> findNeighbour $ graph {currentX = x}
  where
    vs = vertices graph

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

grow :: Graph -> Graph
grow graph =
  let (x, y) = (currentX graph, currentY graph)
      phi = (AF.phi . forest) graph
  in if isOutOfForest graph y
       then let forest' = (forest graph) {AF.phi = Map.insert y x phi}
            in findNeighbour (graph {forest = forest'})
       else augment graph

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

findR :: Graph -> [Vertex] -> [Vertex] -> Int
findR graph px py =
  let (spx, spy) = (Set.fromList px, Set.fromList py)
      isect = px `List.intersect` py
      ro = (AF.ro . forest) graph
  in fromJust $ find (\x -> Graph.get ro x == x) isect

getBlossom :: Graph -> (Int, IntSet, IntSet)
getBlossom graph =
  let (phi, ro)    = ((AF.phi . forest) graph, (AF.ro . forest) graph)
      (px, py)     = (pathToRoot graph X, pathToRoot graph Y)
      r            = findR graph px py
      (pxr, pyr)   = (takeUntil r px, takeUntil r py)
      (spxr, spyr) = (Set.fromList pxr, Set.fromList pyr)
      oddUnion     =
        let (ox, oy) = odds pxr pyr
        in ox `Set.union` oy
      union        = spxr `Set.union` spyr
      filtered     =
        Set.filter (\v -> (Graph.get ro . Graph.get phi) v /= r) oddUnion
  in (r, union, filtered)

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
