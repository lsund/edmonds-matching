{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.State where

import Protolude hiding (State)
import Types
import Edmond.Assoc

import Data.Array
import qualified Data.Map as Map

-- Structures for holding the state of the edmonds algorithm.

----------------------------------------------------------------------------
-- State
--
-- The structure holding the state of the algorithm. mu, phi, ro and scanned are
-- associations, defined as of the specification.
data State = State { mu :: Assoc Vertex Vertex
                   , phi :: Assoc Vertex Vertex
                   , ro :: Assoc Vertex Vertex
                   , scanned :: Assoc Vertex Bool }

-- Initializes the state as of the specification
initialize :: Int -> Int -> State
initialize nv ne = 
    let idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in State (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc sInit)

resetButMu :: Int -> Int -> State -> State
resetButMu nv ne (State mu _ _ _ ) =
    let State _ phi ro scanned = initialize nv ne
    in State mu phi ro scanned

----------------------------------------------------------------------------
-- 'Usual' Graph properties

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

matching :: State -> [(Int, Int)]
matching state = zip (Map.keys m) (Map.elems m)
    where m = (dict . mu) state

