
module Data.AlternatingForest where

import Protolude

import Data.Graph
import Data.IntMap.Strict as Map

-- An alternating forest to be used with Edmonds Blossom Algorithm
-- described by the three maps Mu, Phi and Ro. Mu and Phi are used to
-- traverse the tree from a vertex to the root and Ro is used to
-- manage the blossoms.
data AlternatingForest = AlternatingForest { mu  :: !(IntMap Vertex)
                                           , phi :: !(IntMap Vertex)
                                           , ro  :: !(IntMap Vertex) }


-- Initializes an Alternating Forest
initialize :: AlternatingForest
initialize = AlternatingForest Map.empty Map.empty Map.empty

