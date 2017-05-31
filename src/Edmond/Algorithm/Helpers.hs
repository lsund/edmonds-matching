{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph
import Edmond.Data.Assoc
import qualified Data.Set as Set
import qualified Edmond.Data.AlternatingForest as AF

type Set = Set.Set

pathToRoot :: Graph -> Vertex -> [Vertex]
pathToRoot graph v = takeWhileDifferent $ iterateEveryOther f g v
    where f = (fun . AF.mu . forest) graph
          g = (fun . AF.phi . forest) graph

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)

isOuter :: Graph -> Vertex -> Bool
isOuter graph x = f x == x || g (f x) /= f x
    where f = (fun . AF.mu . forest) graph
          g = (fun . AF.phi . forest) graph

isInner :: Graph -> Vertex -> Bool
isInner graph x = g (f x) == f x && g x /= x
    where f = (fun . AF.mu . forest) graph
          g = (fun . AF.phi . forest) graph

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = f x /= x && g x == x && g (f x) == f x
    where f = (fun . AF.mu . forest) graph
          g = (fun . AF.phi . forest) graph

