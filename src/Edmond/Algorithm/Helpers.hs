{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph
import qualified Data.Set as Set

type Set = Set.Set

odds2 :: Seq Vertex -> Seq Vertex -> (Seq Vertex, Seq Vertex)
odds2 px py = (every2 2 px, every2 2 py)

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)
