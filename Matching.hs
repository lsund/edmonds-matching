
module Matching (edmonds) where

import Data.Array
import Data.Graph

-- To store a special blossom forest F, we introduce the following data
-- structures:
-- For each vertex x we have three variables mu(x), phi(x), ro(x) with the
-- following properties:
--
-- mu x = x, if x is not covered by M
-- mu x = y, if x is covered by edge (x,y) in M
--
--
-- phi(x) = x, if x is not in the forest or x is the base of a blossom in F
-- phi(x) = y, if x is an inner vertex and (x,y) is an unmatched edge in the
--               forest
-- phi(x) = y, if x is an outer vertex and (x,y) is an unmatched edge
--               according to a M-alternating ear-decomposition of the blossom
--               containing x.
--
--
-- ro(x) = x if x is not an outer vertex
-- ro(x) = y if x is an outer vertex and y is the base of the outer blossom in F
--                containing x
--
--
--  mu(x) is itself if not covered or neighbour if covered
--  phi(x) is ...
--  ro(x) is...

scanned = undefined

edmonds graph =
    let nVertices   = (length . vertices) graph
        nEdges      = (length . edges) graph
        muInit      = array (1, nVertices) [(x, x) | x <- [1..nVertices]]
        phiInit     = array (1, nVertices) [(x, x) | x <- [1..nVertices]]
        roInit      = array (1, nVertices) [(x, x) | x <- [1..nVertices]]
        scannedInit = array 
                        (1, nVertices) 
                        [(x, y) | x <- [1..nVertices], y <- replicate 10 False]
    in
        scannedInit

