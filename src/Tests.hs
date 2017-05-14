
module Tests where

import Test.HUnit

import Edmond.State as State
import Edmond.Algorithm
import Protolude
import DIMACSParser
import qualified Data.Graph as Graph
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
                            length $ Graph.vertices x)
test02 = TestCase (do x <- fileToGraph "data/K2.dmx"
                      assertEqual 
                            "Should be 1 edge"
                            1 $
                            length $ Graph.edges x)

test03 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                            "Should be 4 vertices"
                            4 $
                            length $ Graph.vertices x)
test04 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                          "Should be 6 edges"
                          6 $
                          length $ Graph.edges x)

----------------------------------------------------------------------------
-- Augment

augmentedOnceState graph = 
    let lv                  = length . Graph.vertices
        le                  = length . Graph.edges
        state               = State.initialize (lv graph) (le graph)
        ((x, y), state')    = findGrowth graph state
        ((x', y'), state'') = grow graph ((x, y), state')
    in augment graph ((x', y'), state'')

test05 = TestCase (do 
    graph  <- fileToGraph "data/K4.dmx"
    let edges = State.edges $ augmentedOnceState graph
    assertEqual "Should be augmented" True ((1,4) `elem` edges))

----------------------------------------------------------------------------
-- All Tests

tests = TestList [test01, test02, test03, test04, test05]

