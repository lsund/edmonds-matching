
module Edmond.Data.AlternatingForest where

import Protolude
import qualified Data.Graph
import Data.HashMap.Strict as HashMap
import Control.Monad.ST
import Data.HashTable.ST.Basic

type Vertex = Data.Graph.Vertex
type Edge = Data.Graph.Edge

data AlternatingForest s = 
    AlternatingForest { mu :: HashTable s Vertex Vertex
                       , phi :: HashTable s Vertex Vertex
                       , ro :: HashTable s Vertex Vertex }

initialize :: Data.Graph.Graph -> ST s (AlternatingForest s)
initialize rep = do 
    let nv = (length . Data.Graph.vertices) rep
    init <- new 
    return $ AlternatingForest init init init

