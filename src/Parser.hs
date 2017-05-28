{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Parser where

import Protolude
import Data.Graph
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

data DimacsEntry = DimacsSize     { nvertices :: Int
                                  , nedges    :: Int }
                 | DimacsEdge     { first     :: Int
                                  , second    :: Int }
                 | DimacsComment  { content   :: Text } deriving (Show, Eq)

data Optima = Optima { path :: FilePath
                     , optima :: Int } 
            | Void deriving (Show)

class Parseable a where
    parse :: [Text] -> a

instance Parseable Optima where
    parse [x, y]     = Optima (Text.unpack x ++ ".dmx") (textToInt y)
    parse ("c" : xs) = Void
    parse []         = Void

instance Parseable DimacsEntry where
    parse ["p", "edge", x, y]  = DimacsSize (textToInt x) (textToInt y)
    parse [c, x, y]
        | c == "e" || c == "a" = DimacsEdge (textToInt x) (textToInt y)
    parse ("c" : xs)           = DimacsComment $ Text.unwords xs
    parse []                   = DimacsComment ""

textToInt :: Text -> Int
textToInt x =
    case Text.decimal x of
        Left a -> undefined
        Right b -> fst b

loadLines :: [Text] -> [[Text]]
loadLines = map Text.words

parseFile :: FilePath -> IO [[Text]]
parseFile path = fmap (loadLines . Text.lines) (readFile path)

fileToGraph :: FilePath -> IO Graph
fileToGraph path    = do
    content <- parseFile path
    let dimacs = map parse content
        dimacsSize  = fromMaybe undefined $ head [x | x@DimacsSize {} <- dimacs]
        dimacsEdges = [x | x@DimacsEdge {} <- dimacs]
        edges       = map (\(DimacsEdge x y) -> (x, y)) dimacsEdges
        bounds      = (1, nvertices dimacsSize)
    return $ buildG bounds edges
