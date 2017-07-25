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
findRoot :: ST s (Graph s) -> ST s (Graph s)
findRoot graphST = do
    graph' <- graphST
    vs <- vertices graphST
    mx <-  findM (\x -> do
                        xScanned <- isScanned graphST x
                        xOuter <- isOuter graphST x
                        return $
                            if not xScanned && xOuter then
                                Just x
                            else Nothing) vs
    case mx of 
            Nothing -> graphST
            Just x -> do
                let graphST' = updateX graph' x
                findNeighbour graphST'
            

findNeighbour :: ST s (Graph s) -> ST s (Graph s)
findNeighbour graphST = do
    graph <- graphST
    let x = currentX graph
    nbs <- neighbours graphST x
    let pred y = do
            roy <- getVertex graphST Ro y
            rox <- getVertex graphST Ro x
            yIsOuter <- isOuter graphST y
            yIsOutOfForest <- isOutOfForest graphST y
            return $ 
                if (roy /= rox && yIsOuter) || yIsOutOfForest then
                    Just y 
                else 
                    Nothing
    my <- findM pred nbs
    case my of
        Nothing -> findRoot $ updateScanned graphST (x, True)
        Just y -> do
            let graphST'= updateY graph y
            grow graphST'

grow :: ST s (Graph s) -> ST s (Graph s)
grow graph = do
    graph' <- graph
    let x = currentX graph'
        y = currentY graph'
    yIsOutOfForest <- isOutOfForest graph y
    if yIsOutOfForest then
        findNeighbour $ updateSingle graph Phi (y, x)
    else 
        augment graph

augment :: ST s (Graph s) -> ST s (Graph s)
augment graph = do
    graph' <- graph
    let x = currentX graph'
        y = currentY graph'
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
            updateSymmetric graph' Mu ((x, y) : ou')
            let graph'' = reset (return graph')
            findRoot graph''
        else shrink graph exs oxs eys oys xs ys isect

shrink :: ST s (Graph s)
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> Set Vertex
       -> ST s (Graph s)
shrink graphST exs oxs eys oys xs ys isect = do
    graph <- graphST
    mr <- findM (\x -> do 
                        rox <- getVertex graphST Ro x
                        return $ if rox == x then Just x else Nothing)
                (Set.toList isect)
    let x = currentX graph
        y = currentY graph
        r = fromJust mr
    (exsr, oxsr) <- pathToR graphST x r
    (eysr, oysr) <- pathToR graphST y r
    let xsr      = exsr `Set.union` oxsr
        ysr      = eysr `Set.union` oysr
        u        = xsr `Set.union` ysr
        ou       = oxsr `Set.union` oysr
    filtered <- foldM
                    (\acc v -> do
                        v' <- getVertex graphST Phi v
                        v'' <- getVertex graphST Ro v'
                        return $ if v'' /= r then Set.insert v acc else acc)
                    Set.empty
                    ou
    zipped <- foldM 
                (\acc x -> do
                    x' <- getVertex graphST Phi x
                    return $ (x', x) : acc)
                []
                filtered
    updateSymmetric graph Phi zipped
    let graphST' = return graph
    rox <- getVertex graphST' Ro x
    roy <- getVertex graphST' Ro y
    let graphST''   = if rox /= r 
                        then updateSingle graphST' Phi (x, y)
                        else graphST'
        graphST'''  = if roy /= r
                        then updateSingle graphST'' Phi (y, x)
                        else graphST''
    vs <- vertices graphST'''
    keys' <-  filterM 
                (\x -> do
                    rox <- getVertex graphST''' Ro x
                    return $ rox `elem` u) 
                vs
    let zipped' = zip keys' (replicate (length keys') r)
    graph''' <- graphST'''
    update graph''' Ro zipped'
    findNeighbour (return graph''')

edmonds :: Data.Graph.Graph -> IO ()
edmonds rep = do
    let init =  Graph.initialize rep
        maximal = maximalMatching init
        graph = loadMatching init maximal
        graph' = findRoot graph
        maximum = runST $ toMatching graph'
    print $ length maximum 

