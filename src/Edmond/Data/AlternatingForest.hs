
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import qualified Data.HashTable.ST.Basic as HashTable

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type HashTable = HashTable.HashTable

data AlternatingForest s = 
    AlternatingForest {  mu :: !(HashTable s Vertex Vertex)
                       , phi :: !(HashTable s Vertex Vertex)
                       , ro :: !(HashTable s Vertex Vertex) }

initialize :: ST s (AlternatingForest s)
initialize = do
    mu <- HashTable.new
    phi <- HashTable.new
    ro <- HashTable.new
    return $ AlternatingForest mu phi ro

reset :: AlternatingForest s -> ST s (AlternatingForest s)
reset forest = do
    phi <- HashTable.new
    ro <- HashTable.new
    return $ AlternatingForest (mu forest) phi ro
