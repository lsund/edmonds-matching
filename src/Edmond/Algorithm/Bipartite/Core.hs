
module Edmond.Algorithm.Bipartite.Core where

import Protolude
import Data.Maybe
import Edmond.Data.Graph as Graph
import Util
import qualified Data.Graph
import qualified Data.IntSet as Set
import qualified Edmond.Data.AlternatingForest as AF
import Edmond.Algorithm.General.Helpers
import qualified Data.IntMap as Map

maximumMatching :: GraphRepresentation -> [Edge]
maximumMatching rep =
  let init = Graph.initialize rep
  in toMatching $ findRoot init

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
  let pred' = isOuter graph
      pred'' = isOutOfForest graph
      pred y = pred'' y || pred' y
      nbs = neighbours graph x
      found = find pred nbs
  in case found of
       Nothing ->
         let scanned' = Set.insert x $ scanned graph
         in findRoot (graph {scanned = scanned'})
       Just y -> grow (graph {currentY = y})
  where
    x = currentX graph
    ro = Graph.get $ (AF.ro . forest) graph

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
  in 
    let (oddpx, oddpy) = odds px py
        u = oddpx `Set.union` oddpy
        pu = Set.foldr (\x acc -> (x, Graph.get phi x) : acc) [] u
        mu' = insertListSymmetric ((x, y) : (y, x) : pu) mu
        graph' = resetForest graph mu'
     in findRoot graph'
