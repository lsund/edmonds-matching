
module Edmond.Algorithm.Bipartite.Core where

import Protolude
import Edmond.Data.Graph as Graph
import qualified Data.Graph

maximumMatching rep =
  let init = Graph.initialize rep
  in edges init
