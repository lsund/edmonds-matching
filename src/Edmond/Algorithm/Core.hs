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

findM :: (Monad m, Foldable t, Functor t) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)

-- Finds an x such that x is not scanned and x is outer. If success, proceed
-- with calling findGrowth with the found x. If unsuccessful, return the graph
-- of the current state.
findRoot :: Graph s -> ST s ()
findRoot graph = do
    vs <- vertices graph
    mx <-  findM (\x -> do
                        xScanned <- getScanned graph x
                        xOuter <- isOuter graph x
                        return $
                            if not xScanned && xOuter then
                                Just x
                            else Nothing) vs
    case mx of 
            Nothing -> return ()
            Just x -> do
                graph' <- updateX graph x
                findNeighbour graph'
            

findNeighbour :: Graph s -> ST s ()
findNeighbour graph = do
    let x = currentX graph
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
        Nothing -> do
            updateScanned graph (x, True)
            findRoot graph
        Just y -> do
            graph' <- updateY graph y
            grow graph'

grow :: Graph s -> ST s ()
grow graph = do
    let x = currentX graph
        y = currentY graph
    yIsOutOfForest <- isOutOfForest graph y
    if yIsOutOfForest then do
        updateSingle graph Phi (y, x)
        findNeighbour graph
    else 
        augment graph

augment :: Graph s -> ST s ()
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
            updateSymmetric graph Mu ((x, y) : ou')
            graph' <- reset graph
            findRoot graph'
        else shrink graph exs oxs eys oys xs ys isect

shrink :: Graph s
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> ST s ()
shrink graph exs oxs eys oys xs ys isect = do
    mr <- findM (\x -> do 
                        rox <- getVertex graph Ro x
                        return $ if rox == x then Just x else Nothing)
                (Set.toList isect)
    let x = currentX graph
        y = currentY graph
        r = fromJust mr
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
                        return $ if v'' /= r then Set.insert v acc else acc)
                    Set.empty
                    ou
    zipped <- foldM 
                (\acc x -> do
                    x' <- getVertex graph Phi x
                    return $ (x', x) : acc)
                []
                filtered
    updateSymmetric graph Phi zipped
    rox <- getVertex graph Ro x
    roy <- getVertex graph Ro y
    when (rox /= r) $
        updateSingle graph Phi (x, y)
    when (roy /= r) $
        updateSingle graph Phi (y, x)
    vs <- vertices graph
    keys' <-  filterM 
                (\x -> do
                    rox <- getVertex graph Ro x
                    return $ rox `elem` u) 
                vs
    let zipped' = zip keys' (replicate (length keys') r)
    update graph Ro zipped'
    findNeighbour graph


runAlgorithm :: ST s (Graph s) -> ST s (Graph s)
runAlgorithm graph = do
    graph' <- graph
    findRoot graph'
    return graph'

edmonds :: Data.Graph.Graph -> IO ()
edmonds rep = do
    let init =  Graph.initialize rep
        maximal = maximalMatching init
        graph = loadMatching init maximal
        graph' = runAlgorithm graph
        maximum = runST $ toMatching graph'
    print $ length maximum 

