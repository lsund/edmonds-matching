
module Edmond.State where

import Protolude hiding (State)
import Data.Graph
import Data.Maybe
import qualified Data.Map as Map

data VertexAssoc = VertexAssoc { map :: Map Vertex Vertex
                   , fun :: Vertex -> Vertex }


data EdmondFunction = Mu | Phi | Ro

data State = State { mu :: VertexAssoc
                   , phi :: VertexAssoc
                   , ro :: VertexAssoc
                   , scanned :: Map Vertex Bool }

assocToFun :: Map Vertex Vertex -> (Vertex -> Vertex)
assocToFun m x = fromJust $ Map.lookup x m 

initialize :: Graph -> State
initialize graph = 
    let nv    = (length . vertices) graph
        ne    = (length . edges) graph
        idMap = Map.fromList [(x, x) | x <- [1..nv]]
        sInit = Map.fromList [(x, y) | x <- [1..nv], y <- replicate nv False]
    in State (VertexAssoc idMap (assocToFun idMap))
             (VertexAssoc idMap (assocToFun idMap))
             (VertexAssoc idMap (assocToFun idMap))
             sInit

