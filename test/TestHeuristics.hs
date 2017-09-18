module TestHeuristics where

import Protolude
import Parser
import Util
import Edmond.Algorithm.Heuristics.MaximalMatching
import Edmond.Algorithm.Heuristics.ExpandContract
import qualified Edmond.Data.Graph.Core as Graph
import Test.HUnit

testMaximalMatching :: (FilePath, Graph) -> Test
testMaximalMatching (fpath, graph) =
    let matching = maximalMatching graph
    in
      TestCase $ assertEqual
      ("maximalMatching: file: " ++ fpath ++ " Should be a matching")
      True (isMatching matching)

testExpandContract :: (FilePath, Graph) -> Test
testExpandContract (fpath, graph) =
    let matching = expandContract graph
    in
      TestCase $ assertEqual
      ("expandContract: file: " ++ fpath ++ " Should be a matching")
      True (isMatching matching)

isMatchingTests :: ((FilePath, Graph) -> Test) -> IO Test
isMatchingTests heuristic = do
    content <- parseFile "data/optima-stripped.txt"
    let optimas = [x | x@Optima {} <- map parse content]
        files   = map Parser.path optimas
        paths   = map ("data/graphs/" ++) files
    reps <- mapM Parser.fileToGraph paths
    let graphs = map Graph.initialize reps
        tests = map heuristic (zip paths graphs)
    return $ TestList tests

mHeuristicTests :: IO Test
mHeuristicTests = do
    maximalMatchingTests' <- isMatchingTests testMaximalMatching
    expandContractTests' <- isMatchingTests testExpandContract
    return $ TestList [maximalMatchingTests', expandContractTests']
