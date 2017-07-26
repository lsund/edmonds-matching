module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph as Graph
import qualified Data.Set as Set

-- type Set = Set.Set

pathToRoot :: Graph s -> Vertex ->  ST s (Set Vertex, Set Vertex)
pathToRoot graph v = do
    let walkToRoot' v True evens odds = do
            v' <- Graph.getVertex graph Mu v
            if v == v' then
                return (Set.insert v evens, odds)
            else
                walkToRoot' v' False (Set.insert v evens) odds 
        walkToRoot' v False evens odds = do
            v' <- Graph.getVertex graph Phi v
            if v == v' then
                return (evens, Set.insert v odds)
            else
                walkToRoot' v' True evens (Set.insert v odds) 
    walkToRoot' v True Set.empty Set.empty

pathToR :: Graph s -> Vertex -> Vertex ->  ST s (Set Vertex, Set Vertex)
pathToR graph v r = do
    let pathToR' v r True evens odds = do
            v' <- Graph.getVertex graph Mu v
            if v == r then
                return (Set.insert v evens, odds)
            else
                pathToR' v' r False (Set.insert v evens) odds 
        pathToR' v r False evens odds = do
            v' <- Graph.getVertex graph Phi v
            if v == r then
                return (evens, Set.insert v odds)
            else
                pathToR' v' r True evens (Set.insert v odds) 
    pathToR' v r True Set.empty Set.empty

----------------------------------------------------------------------------
-- Used by Core.hs

isOuter :: Graph s -> Vertex -> ST s Bool
isOuter graph x = do
    let  mu = Graph.getVertex graph Mu
         phi = Graph.getVertex graph Phi
    mux <- mu x
    phimux <- phi mux
    return $ mux == x || phimux /= mux

isOutOfForest :: Graph s -> Vertex -> ST s Bool
isOutOfForest graph x = do
    let  mu = Graph.getVertex graph Mu
         phi = Graph.getVertex graph Phi
    mux <- mu x
    phix <- phi x
    phimux <- phi mux
    return $ mux /= x && phix == x && phimux == mux
