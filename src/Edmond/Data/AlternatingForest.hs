
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import Data.HashMap.Strict as HashMap

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest = AlternatingForest { mu :: HashMap Vertex Vertex
                                           , phi :: HashMap Vertex Vertex
                                           , ro :: HashMap Vertex Vertex }

get :: HashMap Vertex Vertex -> Vertex -> Vertex 
get = (!)

initialize :: Data.Graph.Graph -> AlternatingForest
initialize rep = 
    let nv = (length . Data.Graph.vertices) rep
        idMap = HashMap.fromList [(x, x) | x <- [1..nv]]
    in AlternatingForest idMap idMap idMap

