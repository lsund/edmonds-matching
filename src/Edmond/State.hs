
module Edmond.State where

import Protolude hiding (State)
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

data Assoc a b = Assoc { dict :: Map a b
                       , fun :: a -> b }

data State = State { mu :: Assoc Vertex Vertex
                   , phi :: Assoc Vertex Vertex
                   , ro :: Assoc Vertex Vertex
                   , scanned :: Assoc Vertex Bool }

assocToFun :: Ord a => Map a b -> (a -> b)
assocToFun m x = fromJust $ Map.lookup x m 

initialize :: Graph -> State
initialize graph = 
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in State (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc idMap)
             (makeAssoc sInit)

makeAssoc m = Assoc m (assocToFun m)
