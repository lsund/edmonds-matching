
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

initialize :: AlternatingForest s
initialize = AlternatingForest HashTable.new HashTable.new HashTable.new

reset :: AlternatingForest s -> AlternatingForest s
reset forest = AlternatingForest (mu forest) HashTable.new HashTable.new
