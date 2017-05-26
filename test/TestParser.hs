
module TestParser where

import Protolude
import Test.HUnit

import Edmond.Data.Graph as Graph
import qualified Data.Graph
import Parser

test00 = TestCase (do x <- fileToGraph "data/graphs/K2.dmx"
                      assertEqual 
                            "Should be 2 vertices"
                            2 $
                            length $ Data.Graph.vertices x)
test01 = TestCase (do x <- fileToGraph "data/graphs/K2.dmx"
                      assertEqual 
                            "Should be 1 edge"
                            1 $
                            length $ Data.Graph.edges x)

test02 = TestCase (do x <- fileToGraph "data/graphs/K4.dmx"
                      assertEqual 
                            "Should be 4 vertices"
                            4 $
                            length $ Data.Graph.vertices x)
test03 = TestCase (do x <- fileToGraph "data/graphs/K4.dmx"
                      assertEqual 
                          "Should be 6 edges"
                          6 $
                          length $ Data.Graph.edges x)

dimacsTests = TestList [test00, test01, test02, test03]
