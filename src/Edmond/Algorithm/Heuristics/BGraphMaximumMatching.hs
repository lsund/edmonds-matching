
module Edmond.Algorithm.Heuristics.BGraphMaximumMatching where

import Protolude
import Data.List as List

import Edmond.Data.Graph.Core as Graph

graphToBGraph graph = es
    where
        vs = vertices graph
        es = edges graph
        len = length vs
        max = List.last vs
        vs' = zip vs $ take len $ iterate succ (succ max)
