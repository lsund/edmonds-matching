
module Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import Data.IntMap.Strict as Map

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest = AlternatingForest { mu :: !(IntMap Vertex)
                                           , phi :: !(IntMap Vertex)
                                           , ro :: !(IntMap Vertex) }


initialize :: AlternatingForest
initialize = AlternatingForest Map.empty Map.empty Map.empty

