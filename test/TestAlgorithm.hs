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
        , "data/K3.dmx"
        , "data/C5.dmx"
        , "data/K2-v2.dmx"
        , "data/K4-v2.dmx"
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
                  ]

----------------------------------------------------------------------------
-- Utils

checkIfMatching graph = assertBool "" $ isMatching $ matching graph

checkMatchingLen exp graph = assertEqual "" exp $ length $ matching graph

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

