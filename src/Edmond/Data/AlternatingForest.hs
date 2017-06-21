
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import Data.Map as Map

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest = AlternatingForest { mu :: Map Vertex Vertex
                                           , phi :: Map Vertex Vertex
                                           , ro :: Map Vertex Vertex }


initialize :: Data.Graph.Graph -> AlternatingForest
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        ne = (length . Data.Graph.edges) rep
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
    in AlternatingForest idMap idMap idMap

resetButMu :: Data.Graph.Graph -> Map Vertex Vertex -> AlternatingForest
resetButMu rep mu' =
    let new = initialize rep
    in new { mu = mu' }

