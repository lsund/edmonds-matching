
module Edmond.Data.AlternatingForest where

import Protolude
import Edmond.Data.Assoc
import qualified Data.Graph
import Data.Map as Map

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest = AlternatingForest { mu :: Assoc Vertex Vertex
                                           , phi :: Assoc Vertex Vertex
                                           , ro :: Assoc Vertex Vertex }


initialize :: Data.Graph.Graph -> AlternatingForest
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        idAssoc = makeAssoc idMap
    in AlternatingForest idAssoc idAssoc idAssoc

resetButMu :: Data.Graph.Graph -> AlternatingForest -> AlternatingForest
resetButMu rep forest =
    let new = initialize rep
    in new { mu = mu forest }

