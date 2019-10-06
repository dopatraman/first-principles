module Lib (Lib.init, runGame) where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.Random (Random, randomRIO)
import Game

init :: [Char] -> Game
init word = Game (freshState word) (Guesses [])
  where freshState w = GameState (map (\c -> Empty c) w)

guess :: Char -> Game -> Game
guess c (Game (GameState letters) (Guesses guesses)) = Game newState newGuesses
  where newState = GameState $ reveal c letters
        newGuesses = Guesses (c:guesses)

reveal :: Char -> [Letter] -> [Letter]
reveal c letters = map resolve letters
  where resolve a@(Solved _) = a
        resolve (Empty c')
          | c == c' = Solved c'
          | otherwise = Empty c'

gameOver :: Guesses -> Bool
gameOver (Guesses g) = length g > 10

runGame :: Game -> IO ()
runGame g@(Game _ guesses)
  | gameOver guesses = putStrLn "You lose!" >> exitSuccess
  | otherwise = forever $ do
      putStrLn (show g) >> putStrLn "Guess a letter: "

      c <- getLine
      case c of
        [c'] -> runGame $ guess c' g
        _   -> putStrLn "Your guess must be a single character"
