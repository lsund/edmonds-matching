
module Tests where

import Test.HUnit

import Edmond.Graph as Graph
import Edmond.Algorithm
import Protolude
import DIMACSParser
import qualified Data.Graph
import Data.Array
import Data.List
import qualified Data.Map as Map

----------------------------------------------------------------------------
-- utilities

sameElements xs ys = null $ xs \\ ys

----------------------------------------------------------------------------
-- DIMACS parser

test01 = TestCase (do x <- fileToGraph "data/K2.dmx"
                      assertEqual 
                            "Should be 2 vertices"
                            2 $
                            length $ Data.Graph.vertices x)
test02 = TestCase (do x <- fileToGraph "data/K2.dmx"
                      assertEqual 
                            "Should be 1 edge"
                            1 $
                            length $ Data.Graph.edges x)

test03 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                            "Should be 4 vertices"
                            4 $
                            length $ Data.Graph.vertices x)
test04 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                          "Should be 6 edges"
                          6 $
                          length $ Data.Graph.edges x)

----------------------------------------------------------------------------
-- All Tests

tests = TestList [test01, test02, test03, test04]

