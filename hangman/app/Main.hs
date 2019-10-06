module Main where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import Data.Char (toLower)
import Lib (init, runGame)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter a word: "
  w <- getLine
  runGame . Lib.init $ w
