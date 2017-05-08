
module Edmond.State where

import Protolude hiding (State)
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

-- Structures for holding the state of the edmonds algorithm.
--
-- An Assoc is an association between two enteties. It exists to couple the two
-- associations: a dictionary and a function. The dictionary is a map from the
-- first to the second entity. The function is the map applied as a function
-- from the first to the second entity.
data Assoc a b = Assoc { dict :: Map a b
                       , fun :: a -> b }

-- Given a dictionary, creates a function
assocToFun :: Ord a => Map a b -> (a -> b)
assocToFun m x = fromJust $ Map.lookup x m 

-- Given a map, creates an association
makeAssoc :: Ord a => Map a b -> Assoc a b
makeAssoc m = Assoc m (assocToFun m)

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

