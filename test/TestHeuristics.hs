module TestHeuristics where

import Protolude
import Parser
import Util
import Edmond.Algorithm.Heuristics.MaximalMatching
import Edmond.Algorithm.Heuristics.ExpandContract
import qualified Edmond.Data.Graph.Core as Graph
import Test.HUnit

testMaximalMatching :: Graph -> Test
testMaximalMatching graph =
    let matching = maximalMatching graph
    in TestCase $ assertEqual "MaximalMatching: Should be a matching" True (isMatching matching)

testExpandContract :: Graph -> Test
testExpandContract graph =
    let matching = expandContract graph
    in TestCase $ assertEqual "expandContract: Should be a matching" True (isMatching matching)

isMatchingTests :: (Graph -> Test) -> IO Test
isMatchingTests heuristic = do
    content <- parseFile "data/optima-stripped.txt"
    let optimas = [x | x@Optima {} <- map parse content]
        files   = map Parser.path optimas
        paths   = map ("data/graphs/" ++) files
    reps <- mapM Parser.fileToGraph paths
    let graphs = map Graph.initialize reps
        tests = map heuristic graphs
    return $ TestList tests

mHeuristicTests :: IO Test
mHeuristicTests = do
    maximalMatchingTests' <- isMatchingTests testMaximalMatching
    expandContractTests' <- isMatchingTests testExpandContract
    return $ TestList [maximalMatchingTests', expandContractTests']
