{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Edmond.Algorithm.Core where

import Util
import Edmond.Data.Graph as Graph
import qualified Edmond.Data.AlternatingForest as AF
import Edmond.Algorithm.Helpers
import Edmond.Algorithm.Heuristics

import Protolude
import Data.Maybe
import qualified Data.Graph
import qualified Data.Set as Set
import Data.Map.Strict ((!))

-- Finds an x such that x is not scanned and x is outer. If success, proceed
-- with calling findGrowth with the found x. If unsuccessful, return the graph
-- of the current state.
findRoot :: Graph -> Graph
findRoot graph =
    let mx = find (\x -> all ($ x) [not . isScanned graph, isOuter graph]) vs
    in case mx of 
            Nothing -> graph
            Just x -> findNeighbour $ graph { currentX = x }
    where vs = vertices graph
            

-- Map.assocs has RT O(n)
-- At this point, we need to decide where to grow our tree.
-- Finds two vertices (x, y) such that x is an outer vertex with scanned(x) =
-- false, y is a neighbour of x such that y is out-of-forest or y is outer and
-- phi(x) =/ phi(y). (x, y) represents the edge, which we can use to grow the
-- tree
--
-- Given a graph and a vertex x, finds a neighbour y of x such that y is either
-- out-of-forest or (y is outer and ro(y) =/ ro(x)
findNeighbour :: Graph -> Graph
findNeighbour graph =
    let pred' y = isOuter graph y && 
                       ((!) . AF.ro . forest) graph y 
                    /= ((!) . AF.ro . forest) graph x
        pred'' = isOutOfForest graph
        pred y = pred'' y || pred' y
        nbs = neighbours graph x
        found = find pred nbs
    in case found of
        Nothing ->
            let scanned' = adjustMap x True $ scanned graph
            in findRoot (graph { scanned = scanned' })
        Just y -> grow (graph { currentY = y })
    where
        x = currentX graph

grow :: Graph -> Graph
grow graph = 
    if isOutOfForest graph y
        then 
            let phi' = adjustMap y x m
                forest' = (forest graph) { AF.phi = phi' }
            in findNeighbour (graph { forest = forest' })
        else augment graph
    where
        m = (AF.phi . forest) graph
        x = currentX graph
        y = currentY graph

augment :: Graph -> Graph
augment graph = 
    let ((epx, opx), (epy, opy)) = (pathToRoot graph x, pathToRoot graph y)
        px = epx `Set.union` opx
        py = epy `Set.union` opy
        isect = px `Set.intersection` py
    in if null isect
        then
            let u = opx `Set.union` opy
                pu = foldr (\x acc -> (x, phi ! x) : acc) [] u
                mu' = adjustMapFor2 ((x, y) : (y, x) : pu) mu
                graph' = resetForest graph mu'
            in findRoot graph'
        else shrink graph
    where
        x  = currentX graph
        y  = currentY graph
        mu  = (AF.mu . forest) graph
        phi  = (AF.phi . forest) graph

shrink :: Graph -> Graph
shrink graph = 
    let 
        ((espx, ospx), (espy, ospy))  = (pathToRoot graph x, pathToRoot graph y)
        spx = espx `Set.union` ospx
        spy = espy `Set.union` ospy
        isect          = spx `Set.intersection` spy
        r              = fromJust $ find (\x -> ro ! x == x) isect
        ((espxr, ospxr), (espyr, ospyr))  = (pathToR graph x r, pathToR graph y r)
        spxr      = espxr `Set.union` ospxr
        spyr      = espyr `Set.union` ospyr
        u        = spxr `Set.union` spyr
        ou       = ospxr `Set.union` ospyr
        filtered = Set.filter (\v -> ((ro !) . (phi !)) v /= r) ou
        zipped   = foldr (\x acc ->  (phi ! x, x) : acc) [] filtered
        phi'     = adjustMapFor2 zipped phi
        phi''    = symmetricUpdate (ro !) r x y phi'
        keys'    = filter (\x -> (ro !) x `elem` u) (vertices graph)
        ro'      = adjustMapFor keys' (replicate (length keys') r) ro
        forest'  = (forest graph) { AF.phi = phi''
                                  , AF.ro  = ro'
                                  }
    in findNeighbour $ graph { forest = forest' }
    where
        x = currentX graph
        y = currentY graph
        phi = (AF.phi . forest) graph
        ro = (AF.ro . forest) graph


edmonds :: Data.Graph.Graph -> IO [Edge]
edmonds rep =
    let init = Graph.initialize rep
        matching = maximalMatching init
        graph = loadMatching init matching
        graph' = findRoot graph
    in return $ toMatching graph'

-- edmonds :: Data.Graph.Graph -> IO [Edge]
-- edmonds rep =
--     let init = Graph.initialize rep
--         graph = findRoot init
--     in return $ toMatching graph
