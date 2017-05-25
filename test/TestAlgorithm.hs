module TestAlgorithm where

import Protolude
import Test.HUnit
import Util

import Edmond.Data.Graph as Graph
import DIMACSParser
import Edmond.Algorithm.Core

----------------------------------------------------------------------------
-- Data

paths = [ "data/P4.dmx"
        , "data/K2.dmx"
        , "data/K4.dmx"
        , "data/bipartite-simple.dmx"
        , "data/bipartite.dmx"
        ]

expectedLengths = [ 2
                  , 1
                  , 2
                  , 2
                  , 4
                  ]

----------------------------------------------------------------------------
-- Utils

checkIfMatching graph = assertBool "" $ isMatching $ matching graph

checkMatchingLen exp graph = assertEqual "" exp $ length $ matching graph

testIfMatching path = TestCase  (do rep <- fileToGraph path
                                    let init = Graph.initialize rep
                                        (_, graph) = findRoot init
                                    checkIfMatching graph)

testMatchingLen (path, len) = TestCase  (do rep <- fileToGraph path
                                            let init = Graph.initialize rep
                                                (_, graph) = findRoot init
                                            checkMatchingLen len graph)

----------------------------------------------------------------------------
-- tests

tests0 = map testIfMatching paths

tests1 = zipWith (curry testMatchingLen) paths expectedLengths

algoTests = TestList (tests0 ++ tests1)

