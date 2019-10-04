module Main where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import Data.Char (toLower)
import Lib (randomWord, freshPuzzle, runGame)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  runGame $ freshPuzzle (fmap toLower word)
