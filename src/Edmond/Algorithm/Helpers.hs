{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph as Graph

-- type Set = Set.Set

----------------------------------------------------------------------------
-- Used by Core.hs

pathToRoot :: Graph -> Vertex -> (Set Vertex, Set Vertex)
pathToRoot graph v = 
    takeWhileDifferent (iterateEveryOther mu phi v)
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

pathToR :: Graph -> Vertex -> Vertex -> (Set Vertex, Set Vertex)
pathToR graph v r =
    takeUntil r (iterateEveryOther mu phi v)
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)

isOuter graph x = mu x == x || phi (mu x) /= mu x
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isInner :: Graph -> Vertex -> Bool
isInner graph x = phi (mu x) == mu x && phi x /= x
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = mu x /= x && phi x == x && phi (mu x) == mu x
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isScanned :: Graph -> Vertex -> Bool
isScanned graph = s
    where s = Graph.getScanned graph

----------------------------------------------------------------------------
-- 

