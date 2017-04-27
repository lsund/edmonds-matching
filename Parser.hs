
module Parser where

import Data.Char
import Data.List

data DimacsEntry = Dimension Int Int | Edge Int Int | Comment String
    deriving (Show)

parseLine :: [String] -> DimacsEntry
parseLine ["p", "edge", x, y] = Dimension (read x) (read y)
parseLine ["e", x, y]         = Edge (read x) (read y)
parseLine ["a", x, y]         = Edge (read x) (read y)
parseLine ("c" : xs)          = Comment $ unwords xs

linesToEdges :: [String] -> [DimacsEntry]
linesToEdges = map (parseLine . words)

parse :: FilePath -> IO [DimacsEntry]
parse path = fmap (linesToEdges . lines) (readFile path)
