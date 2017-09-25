{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Algorithm.Edmonds.General.Helpers where

import Protolude
import Util
import Data.Graph.Core as Graph
import qualified Data.AlternatingForest as AF
import Data.IntSet as Set
import Data.List as List
import Data.Maybe

data Dimension = X | Y

pathToRoot :: Graph -> Dimension -> [Vertex]
pathToRoot graph dim = 
    case dim of
        X -> takeWhileDifferent (iterate2 mu phi (currentX graph))
        Y -> takeWhileDifferent (iterate2 mu phi (currentY graph))
    where mu = (Graph.get . AF.mu . forest) graph
          phi = (Graph.get . AF.phi . forest) graph

odds :: [Vertex] -> [Vertex] -> (IntSet, IntSet)
odds px py = (every 2 px Set.empty, every 2 py Set.empty)

isOuter :: Graph -> Vertex -> Bool
isOuter graph x = mu x == x || phi (mu x) /= mu x
    where mu = (Graph.get . AF.mu . forest) graph
          phi = (Graph.get . AF.phi . forest) graph

isInner :: Graph -> Vertex -> Bool
isInner graph x = phi (mu x) == mu x && phi x /= x
    where mu = (Graph.get . AF.mu . forest) graph
          phi = (Graph.get . AF.phi . forest) graph

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = mu x /= x && phi x == x && phi (mu x) == mu x
    where mu = (Graph.get . AF.mu . forest) graph
          phi = (Graph.get . AF.phi . forest) graph

isScanned :: Graph -> Vertex -> Bool
isScanned graph v = v `Set.member` scanned graph

sameBlossom :: Graph -> Vertex -> Vertex -> Bool
sameBlossom    graph    x         y       = ro y == ro x
  where
    ro = Graph.get $ (AF.ro . forest) graph

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
