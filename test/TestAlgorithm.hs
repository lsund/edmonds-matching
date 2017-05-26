module TestAlgorithm where

import Protolude
import Test.HUnit
import Util

import Edmond.Data.Graph as Graph
import Parser
import Edmond.Algorithm.Core

dataDir = "data/graphs/"

----------------------------------------------------------------------------
-- Data

paths = [ dataDir ++ "P4.dmx"
        , dataDir ++ "K2.dmx"
        , dataDir ++ "K4.dmx"
        , dataDir ++ "bipartite-simple.dmx"
        , dataDir ++ "bipartite.dmx"
        , dataDir ++ "K3.dmx"
        , dataDir ++ "C5.dmx"
        , dataDir ++ "K2-v2.dmx"
        , dataDir ++ "K4-v2.dmx"
        , dataDir ++ "bipartite-simple-v2.dmx"
        , dataDir ++ "K3-v2.dmx"
        , dataDir ++ "butterfly.dmx"
        , dataDir ++ "butterfly-extended.dmx"
        , dataDir ++ "peterson.dmx"
        ]

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

checkIfMatching graph =
    let m = matching graph
    in assertBool "Should be a matching" $ isMatching m && containsEdges m graph

checkMatchingLen exp graph =
    assertEqual "Should have correct length " exp $ length $ matching graph

testIfMatching path = TestCase  (do rep <- fileToGraph path
                                    let init = Graph.initialize rep
                                    checkIfMatching (findRoot init))

testMatchingLen (path, len) = TestCase  (do rep <- fileToGraph path
                                            let init = Graph.initialize rep
                                            checkMatchingLen len (findRoot init))

----------------------------------------------------------------------------
-- tests

tests0 = map testIfMatching paths

tests1 = zipWith (curry testMatchingLen) paths expectedLengths

algoTests = TestList (tests0 ++ tests1)

