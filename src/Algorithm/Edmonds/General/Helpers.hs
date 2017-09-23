{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Algorithm.Edmonds.General.Helpers where

import Protolude
import Util
import Data.Graph.Core as Graph
import qualified Data.AlternatingForest as AF
import Data.IntSet as Set

data Dimension = X | Y

----------------------------------------------------------------------------
-- Used by Core.hs

pathToRoot :: Graph -> Dimension -> [Vertex]
pathToRoot graph dim = 
    case dim of
        X -> takeWhileDifferent (iterateEveryOther mu phi (currentX graph))
        Y -> takeWhileDifferent (iterateEveryOther mu phi (currentY graph))
    where mu = (Graph.get . AF.mu . forest) graph
          phi = (Graph.get . AF.phi . forest) graph

odds :: [Vertex] -> [Vertex] -> (IntSet, IntSet)
odds px py = (every 2 px Set.empty, every 2 py Set.empty)

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
