module Edmond.Algorithm.Core where

import Edmond.Data.Graph as Graph
import Edmond.Algorithm.Helpers
import Edmond.Algorithm.Heuristics

import Protolude
import Data.Maybe
import qualified Data.Graph
import qualified Data.Set as Set

import Control.Monad.Trans.Maybe

findM :: (Monad m, Foldable t, Functor t) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

-- Finds an x such that x is not scanned and x is outer. If success, proceed
-- with calling findGrowth with the found x. If unsuccessful, return the graph
-- of the current state.
findRoot :: Graph s -> ST s (Graph s)
findRoot graph = do
    let vs = vertices graph
    mx <-  findM (\x -> do
                    xScanned <- getScanned graph x
                    xOuter <- isOuter graph x
                    return $
                        if not xScanned && xOuter then
                            Just x
                        else Nothing) 
                  vs
    case mx of 
        Nothing -> return graph
        Just x -> findNeighbour $ updateX graph (traceShowId x)
            

findNeighbour :: Graph s -> ST s (Graph s)
findNeighbour graph = do
    let x = currentX graph
        nbs = neighbours graph x
        pred y = do
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
        Nothing -> updateScanned graph (x, True) >>= findRoot
        Just y -> grow $ updateY graph y

grow :: Graph s -> ST s (Graph s)
grow graph = do
    let x = currentX graph
        y = currentY graph
    yIsOutOfForest <- isOutOfForest graph y
    if yIsOutOfForest then
        updateSingle graph Phi (y, x) >>= findNeighbour
    else 
        augment graph

augment :: Graph s -> ST s (Graph s)
augment graph = do
    let x = currentX graph
        y = currentY graph
    (exs, oxs) <- pathToRoot graph x
    (eys, oys) <- pathToRoot graph y
    let xs = exs `Set.union` oxs
        ys = eys `Set.union` oys
        isect = xs `Set.intersection` ys
    if null isect
        then do
            let ou = oxs `Set.union` oys
            ou' <- mapM (\x -> do
                            phix <- getVertex graph Phi x
                            return (x, phix))
                        (Set.toList ou)
            updateSymmetric graph Mu ((x, y) : ou') >>= (findRoot . reset)
        else shrink graph

shrink :: Graph s -> ST s (Graph s)
shrink graph = do
    let x = currentX graph
        y = currentY graph
    (exs, oxs) <- pathToRoot graph x
    (eys, oys) <- pathToRoot graph y
    let xs = exs `Set.union` oxs
        ys = eys `Set.union` oys
        isect = xs `Set.intersection` ys
    mr <- findM (\x -> do 
                        rox <- getVertex graph Ro x
                        return $ if rox == x then Just x else Nothing)
                (Set.toList isect)
    let x = currentX graph
        y = currentY graph
        r = fromMaybe undefined mr
    (exsr, oxsr) <- pathToR graph x r
    (eysr, oysr) <- pathToR graph y r
    let xsr      = exsr `Set.union` oxsr
        ysr      = eysr `Set.union` oysr
        u        = xsr `Set.union` ysr
        ou       = oxsr `Set.union` oysr
    filtered <- foldM
                    (\acc v -> do
                        v' <- getVertex graph Phi v
                        v'' <- getVertex graph Ro v'
                        return $ if v'' /= r then v : acc else acc)
                    []
                    ou
    zipped <- foldM 
                (\acc x -> do
                    x' <- getVertex graph Phi x
                    return $ (x', x) : acc)
                []
                filtered
    vGraph <- updateSymmetric graph Phi zipped
    rox <- getVertex vGraph Ro x
    roy <- getVertex vGraph Ro y
    let graph' = 
            if rox /= r then
                updateSingle vGraph Phi (x, y)
            else
                    return graph
    let graph'' =
            if roy /= r then
                updateSingle graph Phi (y, x)
            else
                graph'
    let vs = vertices graph
    vGraph'' <- graph''
    keys' <-  filterM 
                (\x -> do
                    rox <- getVertex vGraph'' Ro x
                    return $ rox `elem` u) 
                vs
    let zipped' = zip keys' (replicate (length keys') r)
    update vGraph'' Ro zipped' >>= findNeighbour

edmonds :: Data.Graph.Graph -> IO ()
edmonds rep = do
    let init =  Graph.initialize rep
        maximal = maximalMatching init
        -- graph = loadMatching init maximal >>= findRoot
        graph = findRoot init
        maximum = runST $ toMatching graph
    print $ length maximum 

