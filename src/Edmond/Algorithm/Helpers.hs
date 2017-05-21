{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Helpers where

import Util
import Edmond.Data.Graph
import Edmond.Data.Vertex
import qualified Data.Set as Set

type Set = Set.Set

odds :: [Vertex] -> [Vertex] -> ([Vertex], [Vertex])
odds px py = (every 2 px, every 2 py)

rootPaths :: Graph
          -> Vertex
          -> Vertex
          -> ([Vertex], [Vertex], Set Vertex, Set Vertex)
rootPaths graph x y = 
    let (px, py) = (pathToRoot graph x, pathToRoot graph y)
    in (px, py, Set.fromList px, Set.fromList py)
        -- converting to set to be able to call
        -- areDisjoint. Bad??

