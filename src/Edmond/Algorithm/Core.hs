module Edmond.Algorithm.Core where

import Util
import Edmond.Data.Graph as Graph
import Edmond.Algorithm.Helpers
import Edmond.Algorithm.Heuristics

import Protolude
import Data.Maybe
import qualified Data.Graph
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.Trans.Maybe

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

-- Finds an x such that x is not scanned and x is outer. If success, proceed
-- with calling findGrowth with the found x. If unsuccessful, return the graph
-- of the current state.
findRoot :: ST s (Graph s) -> ST s (Graph s)
findRoot graph = do
    vs <- vertices graph
    mx <-  findM (\x -> do
                        xScanned <- isScanned graph x
                        xOuter <- isOuter graph x
                        return $
                            if not xScanned && xOuter then
                                Just x
                            else Nothing) vs
    case mx of 
            Nothing -> graph
            Just x -> findNeighbour $ updateX graph x
            

findNeighbour :: ST s (Graph s) -> ST s (Graph s)
findNeighbour graph = do
    graph' <- graph
    let x = currentX graph'
    nbs <- neighbours graph x
    let pred y = do
            roy <- getVertex graph Ro y
            rox <- getVertex graph Ro x
            yIsOuter <- isOuter graph y
            yIsOutOfForest <- isOutOfForest graph y
            return $ 
                if (roy /= rox && yIsOuter) || yIsOutOfForest then
                    Just y 
                else 
                    Nothing
    my <- findM pred nbs
    case my of
        Nothing -> findRoot $ updateScanned graph (x, True)
        Just y -> grow $ updateY graph y

grow :: ST s (Graph s) -> ST s (Graph s)
grow graph = 
    if isOutOfForest graph y
        then findNeighbour $ updateSingle graph Phi (y, x)
        else augment graph
    where
        x = currentX graph
        y = currentY graph

augment :: ST s (Graph s) -> ST s (Graph s)
augment graph = 
    let ((exs, oxs), (eys, oys)) = (pathToRoot graph x, pathToRoot graph y)
        xs = exs `Set.union` oxs
        ys = eys `Set.union` oys
        isect = xs `Set.intersection` ys
    in if null isect
        then
            let ou = oxs `Set.union` oys
                ou' = foldr (\x acc -> (x, getVertex graph Phi x) : acc) [] ou
                graph' = Graph.updateSymmetric graph Mu ((x, y) : ou')
                graph'' = reset graph'
            in findRoot graph''
        else shrink graph exs oxs eys oys xs ys isect
    where
        x  = currentX graph
        y  = currentY graph

-- shrink :: Graph -> Graph
shrink graph exs oxs eys oys xs ys isect = 
    let 
        r       = fromJust $ find (\x -> getVertex graph Ro x  == x) isect
        ((exsr, oxsr), (eysr, oysr)) = (pathToR graph x r, pathToR graph y r)
        xsr      = exsr `Set.union` oxsr
        ysr      = eysr `Set.union` oysr
        u        = xsr `Set.union` ysr
        ou       = oxsr `Set.union` oysr
        filtered = Set.filter 
                    (\v -> 
                        (getVertex graph Ro . getVertex graph Phi)
                        v /= r)
                    ou
        zipped   = foldr (\x acc -> (getVertex graph Phi x, x) : acc) [] filtered
        graph'     = updateSymmetric graph Phi zipped
        graph''  = if getVertex graph' Ro x /= r
                    then updateSingle graph' Phi (x, y)
                    else graph'
        graph'''   = if getVertex graph'' Ro y /= r
                    then updateSingle graph'' Phi (y, x)
                    else graph''
        keys'    = filter (\x -> getVertex graph''' Ro x `elem` u) 
                          (vertices graph''')
        zipped'  = zip keys' (replicate (length keys') r)
        graph'''' = update graph''' Ro zipped'
    in findNeighbour graph''''
    where
        x = currentX graph
        y = currentY graph

edmonds :: Data.Graph.Graph -> IO [Edge]
edmonds rep =
    let init = Graph.initialize rep
        matching = maximalMatching init
        graph = loadMatching init matching
        graph' = findRoot graph
    in return $ toMatching graph'

