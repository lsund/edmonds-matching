module TestAlgorithm where

import Protolude
import Test.HUnit

import Edmond.Data.Graph as Graph
import DIMACSParser
import Edmond.Algorithm.Core

test00 = TestCase   (do rep <- fileToGraph "data/P4.dmx"
                        let init = Graph.initialize rep
                            (_, graph) = findRoot init
                        assertEqual
                            ""
                            [(1,3),(2,4)]
                            (matching graph))

test01 = TestCase   (do rep <- fileToGraph "data/K2.dmx"
                        let init = Graph.initialize rep
                            (_, graph) = findRoot init
                        assertEqual
                            ""
                            1
                            (length $ matching graph))

test02 = TestCase   (do rep <- fileToGraph "data/K4.dmx"
                        let init = Graph.initialize rep
                            (_, graph) = findRoot init
                        assertEqual
                            ""
                            2
                            (length $ matching graph))

algoTests = TestList [test00, test01, test02]

