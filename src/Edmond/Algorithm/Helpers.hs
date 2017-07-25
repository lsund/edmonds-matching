module Edmond.Algorithm.Helpers where

import Protolude
import Util
import Edmond.Data.Graph as Graph
import qualified Data.Set as Set

-- type Set = Set.Set

pathToRoot :: ST s (Graph s) -> Vertex ->  ST s (Set Vertex, Set Vertex)
pathToRoot graph v = do
    let walkToRoot' v True evens odds = do
            v' <- Graph.getVertex graph Mu v
            if v == v' then
                return (Set.insert v evens, odds)
            else
                walkToRoot' v False (Set.insert v evens) odds 
        walkToRoot' v False evens odds = do
            v' <- Graph.getVertex graph Phi v
            if v == v' then
                return (evens, Set.insert v odds)
            else
                walkToRoot' v True evens (Set.insert v odds) 
    walkToRoot' v True Set.empty Set.empty

----------------------------------------------------------------------------
-- Used by Core.hs

-- pathToRoot :: ST s (Graph s) -> Vertex -> ST s (Set Vertex, Set Vertex)
-- pathToRoot graph v = do
--     let mu = Graph.getVertex graph Mu
--         phi = Graph.getVertex graph Phi
--     rootpath <- iterateEveryOther mu phi v
--     return $ takeWhileDifferent rootpath --stuck

pathToR :: ST s (Graph s) -> Vertex -> Vertex -> ST s (Set Vertex, Set Vertex)
pathToR graph v r = do
    rootpath <- iterateEveryOther mu phi v
    return $ takeUntil r rootpath
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isOuter :: ST s (Graph s) -> Vertex -> ST s Bool
isOuter graph x = do
    mux <- mu x
    phimux <- phi mux
    return $ mux == x || phimux /= mux
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isOutOfForest :: ST s (Graph s) -> Vertex -> ST s Bool
isOutOfForest graph x = do
    mux <- mu x
    phix <- phi x
    phimux <- phi mux
    return $ mux /= x && phix == x && phimux == mux
    where mu = Graph.getVertex graph Mu
          phi = Graph.getVertex graph Phi

isScanned :: ST s (Graph s) -> Vertex -> ST s Bool
isScanned graph = s
    where s = Graph.getScanned graph
