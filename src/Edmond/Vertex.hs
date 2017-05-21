
module Edmond.Vertex where

import Types
import Protolude
import Util
import qualified Data.Graph
import Edmond.Graph 
import Edmond.Assoc

pathToRoot :: Graph -> Vertex -> [Vertex]
pathToRoot graph v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther f g
          f = fun $ mu graph
          g = fun $ phi graph

isOuter :: Graph -> Vertex -> Bool
isOuter graph x = f x == x || g (f x) /= f x
    where f = fun $ mu graph
          g = fun $ phi graph

isInner :: Graph -> Vertex -> Bool
isInner graph x = g (f x) == f x && g x /= x
    where f = fun $ mu graph
          g = fun $ phi graph

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest graph x = f x /= x && g x == x && g (f x)== f x
    where f = fun $ mu graph
          g = fun $ phi graph


outers graph = filter (isOuter graph) (Data.Graph.vertices (representation graph))

inners graph = filter (isInner graph) (Data.Graph.vertices (representation graph))

outOfForests graph = filter (isOutOfForest graph) (Data.Graph.vertices (representation graph))
