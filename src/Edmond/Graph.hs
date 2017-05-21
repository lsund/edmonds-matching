{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Graph where

import Protolude
import Types
import Edmond.Assoc

import Data.Array
import qualified Data.Graph
import qualified Data.Map as Map

-- Structures for holding a graph and an associated alternating forest

----------------------------------------------------------------------------
-- Graph
data Graph = Graph { graph :: Data.Graph.Graph
                   , mu :: Assoc Vertex Vertex
                   , phi :: Assoc Vertex Vertex
                   , ro :: Assoc Vertex Vertex
                   , scanned :: Assoc Vertex Bool }

-- Initializes the state as of the specification
initialize :: Data.Graph.Graph -> Int -> Int -> Graph
initialize graph nv ne = 
    let idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in Graph graph 
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc sInit)

resetButMu :: Int -> Int -> Graph -> Graph
resetButMu nv ne (Graph graph mu _ _ _ ) =
    let Graph _ _ phi ro scanned = initialize graph nv ne
    in Graph graph mu phi ro scanned

----------------------------------------------------------------------------
-- 'Usual' Graph properties

neighbours :: Data.Graph.Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

matching :: Graph -> [(Int, Int)]
matching state = zip (Map.keys m) (Map.elems m)
    where m = (dict . mu) state

