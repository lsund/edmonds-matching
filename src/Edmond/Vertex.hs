
module Edmond.Vertex where

import Types
import Protolude
import Util
import Edmond.Graph 
import Edmond.Data.Assoc
import Edmond.Data.AlternatingForest

pathToRoot :: Graph -> Vertex -> [Vertex]
pathToRoot graph v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther f g
          f = (fun . mu . forest) graph
          g = (fun . phi . forest) graph

isOuter :: Graph -> Vertex -> Bool
isOuter graph x = f x == x || g (f x) /= f x
    where f = (fun . mu . forest) graph
          g = (fun . phi . forest) graph

isInner :: Graph -> Vertex -> Bool
isInner graph x = g (f x) == f x && g x /= x
    where f = (fun . mu . forest) graph
          g = (fun . phi . forest) graph

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = f x /= x && g x == x && g (f x)== f x
    where f = (fun . mu . forest) graph
          g = (fun . phi . forest) graph


outers graph = filter (isOuter graph) (vertices graph)

inners graph = filter (isInner graph) (vertices graph)

outOfForests graph = filter (isOutOfForest graph) (vertices graph)
