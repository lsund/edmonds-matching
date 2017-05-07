
module Graph where

import Data.Graph
import Data.Array

neighbours :: Graph -> Vertex -> [Vertex]
neighbours graph v = graph ! v

