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
    let outerAndDisjoint y = isOuter graph y && 
                       ((!) . AF.ro . forest) graph y 
                    /= ((!) . AF.ro . forest) graph x
        nbs = neighbours graph x
        mnb = find (\x' -> outerAndDisjoint x' || isOutOfForest graph x') nbs
    in case mnb of
        Nothing ->
            let scanned' = adjustMap x True $ scanned graph
            in findRoot (graph { scanned = scanned' })
        Just nb -> grow (graph { currentY = nb })
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
    let ((exs, oxs), (eys, oys)) = (pathToRoot graph x, pathToRoot graph y)
        xs = exs `Set.union` oxs
        ys = eys `Set.union` oys
        isect = xs `Set.intersection` ys
    in if null isect
        then
            let ou = oxs `Set.union` oys
                ou' = foldr (\x acc -> (x, phi ! x) : acc) [] ou
                mu' = adjustMapForSymmetric ((x, y) : ou') mu
                graph' = resetForest graph mu'
            in findRoot graph'
        else shrink graph exs oxs eys oys xs ys isect
    where
        x  = currentX graph
        y  = currentY graph
        mu  = (AF.mu . forest) graph
        phi  = (AF.phi . forest) graph

-- shrink :: Graph -> Graph
shrink graph exs oxs eys oys xs ys isect = 
    let 
        r              = fromJust $ find (\x -> ro ! x == x) isect
        ((exsr, oxsr), (eysr, oysr)) = (pathToR graph x r, pathToR graph y r)
        xsr      = exsr `Set.union` oxsr
        ysr      = eysr `Set.union` oysr
        u        = xsr `Set.union` ysr
        ou       = oxsr `Set.union` oysr
        filtered = Set.filter (\v -> ((ro !) . (phi !)) v /= r) ou
        zipped   = foldr (\x acc ->  (phi ! x, x) : acc) [] filtered
        phi'     = adjustMapForSymmetric zipped phi
        phi''    = symmetricUpdate (ro !) r x y phi'
        keys'    = filter (\x -> (ro !) x `elem` u) (vertices graph)
        ro'      = adjustMapFor (zip keys' (replicate (length keys') r)) ro
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

