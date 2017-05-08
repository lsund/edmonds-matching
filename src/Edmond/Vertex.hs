
module Edmond.Vertex where

import Util
import Data.Graph
import Edmond.State

pathToRoot :: State -> Vertex -> [Vertex]
pathToRoot state v = v : takeWhile (/= v) (iterateFG v)
    where iterateFG = iterateEveryOther f g
          f = fun $ mu state
          g = fun $ phi state

isOuter :: State -> Vertex -> Bool
isOuter state x = f x == x || g (f x) /= f x
    where f = fun $ mu state
          g = fun $ phi state

isInner :: State -> Vertex -> Bool
isInner state x = g (f x) == f x && g x /= x
    where f = fun $ mu state
          g = fun $ phi state

isOutOfForest :: State -> Vertex -> Bool
isOutOfForest state x = f x /= x && g x == x && g (f x)== f x
    where f = fun $ mu state
          g = fun $ phi state

