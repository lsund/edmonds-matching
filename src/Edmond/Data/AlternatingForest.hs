
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import qualified Data.HashTable.ST.Basic as HashTable

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge
type HashTable = HashTable.HashTable

data AlternatingForest s = 
    AlternatingForest { mu :: HashTable s Vertex Vertex
                       , phi :: HashTable s Vertex Vertex
                       , ro :: HashTable s Vertex Vertex }

initialize :: Data.Graph.Graph -> ST s (AlternatingForest s)
initialize rep = do 
    let nv = (length . Data.Graph.vertices) rep
    mu <- HashTable.newSized $ nv
    phi <- HashTable.newSized $ nv
    ro <- HashTable.newSized $ nv
    mapM_ (\k -> HashTable.insert mu k k) [1..nv]
    mapM_ (\k -> HashTable.insert phi k k) [1..nv]
    mapM_ (\k -> HashTable.insert ro k k) [1..nv]
    return $ AlternatingForest mu phi ro

reset :: AlternatingForest s -> ST s ()
reset forest = do
    HashTable.mapM_ (\(k, v) -> HashTable.insert (mu forest) k k) (mu forest)
    HashTable.mapM_ (\(k, v) -> HashTable.insert (phi forest) k k) (phi forest)
    HashTable.mapM_ (\(k, v) -> HashTable.insert (ro forest) k k) (ro forest)

