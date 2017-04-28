
module Tests where

import Test.HUnit

import DIMACSParser
import Data.Graph
import Data.Array

test01 = TestCase (do x <- fileToGraph "data/K2.dmx"
                      assertEqual 
                            "Should be 2 vertices"
                            2 $
                            length $ vertices x)
test02 = TestCase (do x <- fileToGraph "data/K2.dmx"
                      assertEqual 
                            "Should be 1 edge"
                            1 $
                            length $ edges x)

test03 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                            "Should be 4 vertices"
                            4 $
                            length $ vertices x)
test04 = TestCase (do x <- fileToGraph "data/K4.dmx"
                      assertEqual 
                          "Should be 6 edges"
                          6 $
                          length $ edges x)

tests = TestList [test01, test02, test03, test04]

