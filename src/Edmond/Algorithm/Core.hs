module Edmond.Algorithm.Core where

import Util
import Edmond.Data.Graph as Graph
import qualified Edmond.Data.AlternatingForest as AF
import Edmond.Algorithm.Helpers
import Edmond.Algorithm.Heuristics

import Protolude
import Data.Maybe
import qualified Data.Graph
import qualified Data.List as List
import Data.Map.Strict ((!))

findRoot :: Graph -> Graph
findRoot graph =
    let mx = find (\x -> all ($ x) [not . isScanned graph, isOuter graph]) vs
    in case mx of 
            Nothing -> graph
            Just x -> findNeighbour $ graph { currentX = x }
    where vs = vertices graph
            
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
    let (px, py) = (pathToRoot graph x, pathToRoot graph y)
        isect = px `List.intersect` py
    in if null isect
        then
            let (oddpx, oddpy) = odds px py
                u = oddpx ++ oddpy
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
    let (px, py)       = (pathToRoot graph x, pathToRoot graph y)
        isect          = px `List.intersect` py
        r              = fromJust $ find (\x -> ro ! x == x) isect
        (pxr, pyr)     = (takeUntil r px, takeUntil r py)
        (oddpx, oddpy) = odds pxr pyr
        union          = pxr `List.union` pyr
        oddUnion       = oddpx `List.union` oddpy
        filtered       = filter (\v -> ((ro !) . (phi !)) v /= r) oddUnion
        phi'           = adjustMapFor (map (phi !) filtered) filtered phi
        phi''          = symmetricUpdate (ro !) r x y phi'
        keys'          = filter (\x -> (ro !) x `elem` union) (vertices graph)
        ro'            = adjustMapFor keys' (repeat r) ro
        forest'        = (forest graph) { AF.phi = phi''
                                        , AF.ro = ro' }
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
