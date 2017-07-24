
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
-- import Data.Map.Strict as Map
import Data.HashMap.Strict as HashMap

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest = AlternatingForest { mu :: HashMap Vertex Vertex
                                           , phi :: HashMap Vertex Vertex
                                           , ro :: HashMap Vertex Vertex }


initialize :: Data.Graph.Graph -> AlternatingForest
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        idMap = HashMap.fromList [(x, x) | x <- [1..nv]]
    in AlternatingForest idMap idMap idMap

