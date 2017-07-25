{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph as Graph

-- type Set = Set.Set

----------------------------------------------------------------------------
-- Used by Core.hs

pathToRoot :: ST s (Graph s) -> Vertex -> ST s (Set Vertex, Set Vertex)
pathToRoot graph v = do
    graph' <- graph
    let mu = Graph.getVertex graph Mu
        phi = Graph.getVertex graph Phi
    let rootpath = iterateEveryOther mu phi v
    return $ takeWhileDifferent rootpath

pathToR :: ST s (Graph s) -> Vertex -> Vertex -> ST s (Set Vertex, Set Vertex)
pathToR graph v r =
    takeUntil r (iterateEveryOther mu phi v)
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)

isOuter :: ST s (Graph s) -> Vertex -> ST s Bool
isOuter graph x = mu x == x || phi (mu x) /= mu x
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isOutOfForest :: ST s (Graph s) -> Vertex -> ST s Bool
isOutOfForest graph x = mu x /= x && phi x == x && phi (mu x) == mu x
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isScanned :: ST s (Graph s) -> Vertex -> ST s Bool
isScanned graph = s
    where s = Graph.getScanned graph


