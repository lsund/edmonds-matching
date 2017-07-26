
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import qualified Data.HashTable.ST.Basic as HashTable

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type HashTable = HashTable.HashTable

data AlternatingForest s = 
    AlternatingForest {  mu :: ST s (HashTable s Vertex Vertex)
                       , phi :: ST s (HashTable s Vertex Vertex)
                       , ro :: ST s (HashTable s Vertex Vertex) }

initialize :: Data.Graph.Graph -> AlternatingForest s
initialize rep =
    let nv = (length . Data.Graph.vertices) rep
        mu = HashTable.new 
        phi = HashTable.new
        ro = HashTable.new
    in AlternatingForest mu phi ro

reset :: AlternatingForest s -> Data.Graph.Graph -> AlternatingForest s
reset forest rep =
    let nv = (length . Data.Graph.vertices) rep
        phi = HashTable.new
        ro = HashTable.new
    in AlternatingForest (mu forest) phi ro
