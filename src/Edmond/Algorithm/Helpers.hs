{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph
import qualified Edmond.Data.AlternatingForest as AF
import Data.HashMap.Strict ((!))

-- type Set = Set.Set

----------------------------------------------------------------------------
-- Used by Core.hs

pathToRoot :: Graph -> Vertex -> (Set Vertex, Set Vertex)
pathToRoot graph v = 
    takeWhileDifferent (iterateEveryOther mu phi v)
    where mu = ((!) . AF.mu . forest) graph
          phi = ((!) . AF.phi . forest) graph

pathToR :: Graph -> Vertex -> Vertex -> (Set Vertex, Set Vertex)
pathToR graph v r =
    takeUntil r (iterateEveryOther mu phi v)
    where mu = ((!) . AF.mu . forest) graph
          phi = ((!) . AF.phi . forest) graph

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)

isOuter graph x = mu x == x || phi (mu x) /= mu x
    where mu = ((!) . AF.mu . forest) graph
          phi = ((!) . AF.phi . forest) graph

isInner :: Graph -> Vertex -> Bool
isInner graph x = phi (mu x) == mu x && phi x /= x
    where mu = ((!) . AF.mu . forest) graph
          phi = ((!) . AF.phi . forest) graph

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = mu x /= x && phi x == x && phi (mu x) == mu x
    where mu = ((!) . AF.mu . forest) graph
          phi = ((!) . AF.phi . forest) graph

isScanned :: Graph -> Vertex -> Bool
isScanned graph = s
    where s = ((!) . scanned) graph

----------------------------------------------------------------------------
-- 

