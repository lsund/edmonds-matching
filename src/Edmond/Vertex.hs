
module Edmond.Vertex where

import Types
import Protolude
import Util
import qualified Data.Graph
import Edmond.Graph 
import Edmond.Assoc

pathToRoot :: Graph -> Vertex -> [Vertex]
pathToRoot state v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther f g
          f = fun $ mu state
          g = fun $ phi state

isOuter :: Graph -> Vertex -> Bool
isOuter state x = f x == x || g (f x) /= f x
    where f = fun $ mu state
          g = fun $ phi state

isInner :: Graph -> Vertex -> Bool
isInner state x = g (f x) == f x && g x /= x
    where f = fun $ mu state
          g = fun $ phi state

isOutOfForest :: Graph -> Vertex -> Bool
isOutOfForest state x = f x /= x && g x == x && g (f x)== f x
    where f = fun $ mu state
          g = fun $ phi state


outers graph state = filter (isOuter state) (Data.Graph.vertices graph)

inners graph state = filter (isInner state) (Data.Graph.vertices graph)

outOfForests graph state = filter (isOutOfForest state) (Data.Graph.vertices graph)
