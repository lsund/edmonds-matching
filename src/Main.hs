module Main where
{-# Language Overloadedstrings #-}
import Prelude ()
import Protolude
import Algorithm.Edmonds.Core
import Algorithm.Heuristics.Core


-- Top level entry point
main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 2 then
    putStrLn ("Usage: ./edmonds-matching gm|ec|no PATH" :: [Char])
  else do
    let (heuristic : path : []) = args
    case heuristic of
      "gm" -> run path GreedyMaximal
      "ec" -> run path ExpandContract
      _    -> run path None
