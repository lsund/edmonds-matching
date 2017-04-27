
module Parser where

import Data.Char

data DimacsEntry = Dimension Int Int | Edge Int Int
    deriving (Show)

parseLine :: [String] -> DimacsEntry
parseLine ["p", "edge", x, y] = Dimension (read x) (read y)
parseLine ["e", x, y]         = Edge (read x) (read y)
parseLine ["a", x, y]         = Edge (read x) (read y)

linesToEdges :: [String] -> [DimacsEntry]
linesToEdges = map (parseLine . words)

parse :: FilePath -> IO [DimacsEntry]
parse path = fmap (linesToEdges . lines) (readFile path)
