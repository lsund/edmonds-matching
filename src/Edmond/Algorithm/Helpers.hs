{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph
import qualified Data.Set as Set

type Set = Set.Set

odds :: Seq Vertex -> Seq Vertex -> (Seq Vertex, Seq Vertex)
odds px py = (every2 2 px, every2 2 py)

