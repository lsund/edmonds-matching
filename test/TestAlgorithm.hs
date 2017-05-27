{-# LANGUAGE OverloadedStrings #-}

module TestAlgorithm where

import Protolude
import Test.HUnit
import Util
import qualified Data.Text as Text

import Edmond.Data.Graph as Graph
import Parser
import Edmond.Algorithm.Core

----------------------------------------------------------------------------
-- Data

dataDir = "data/graphs/"

expectedLengths = [ 2
                  , 1
                  , 2
                  , 2
                  , 4
                  , 1
                  , 2
                  , 1
                  , 2
                  , 2
                  , 1
                  , 2
                  , 3
                  , 5
                  ]

----------------------------------------------------------------------------
-- Utils

checkIfMatching :: Graph -> Assertion
checkIfMatching graph =
    let m = matching graph
    in assertBool "Should be a matching" $ isMatching m && containsEdges m graph

checkMatchingLen :: Int -> Graph -> Assertion 
checkMatchingLen exp graph =
    assertEqual "Should have correct length " exp $ length $ matching graph

testIfMatching :: FilePath -> Test
testIfMatching path = TestCase  (do rep <- fileToGraph path
                                    let init = Graph.initialize rep
                                    checkIfMatching (findRoot init))

testMatchingLen :: (FilePath, Int) -> Test
testMatchingLen (path, len) = TestCase  (do rep <- fileToGraph path
                                            let init = Graph.initialize rep
                                            checkMatchingLen len (findRoot init))

----------------------------------------------------------------------------
-- tests

tests0 :: IO [Test]
tests0 = do
    content <- parseFile "data/optima.txt"
    let optimas = map parse content
        names = map Parser.path optimas
        paths = map ("data/graphs/" ++) names
    return $ map testIfMatching paths

tests1 :: IO [Test]
tests1 = do content <- parseFile "data/optima.txt"
            let optimas = map parse content
                names   = map Parser.path optimas
                paths = map ("data/graphs/" ++) names
                optima  = map Parser.optima optimas
            return $ zipWith (curry testMatchingLen) paths optima

mAlgoTests :: IO Test
mAlgoTests = do
    t0 <- tests0
    t1 <- tests1
    return $ TestList (t0 ++ t1)

