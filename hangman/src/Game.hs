module Game where

import Data.List (intersperse)
import Data.Char (toUpper)
import Control.Monad (forever)

newtype Solution = Solution [Char]
data Letter = Empty | Solved Char
newtype GameState = GameState [Letter]
newtype Guesses = Guesses [Char]

data Game = Game Solution GameState Guesses

instance Show GameState where
  show (GameState []) = ""
  show (GameState (Empty:letters)) = '_':(show (GameState letters))
  show (GameState ((Solved c):letters)) = c:(show (GameState letters))

instance Show Guesses where
  show (Guesses []) = ""
  show (Guesses (g:guesses)) = (toUpper g):(show $ Guesses guesses)

instance Show Game where
  show (Game _ st g) = "Current State: " ++ show st ++ "\nCurrent guesses: " ++ show g

data GameResult = Nil | InProgress | Win | Loss

init :: [Char] -> Game
init word = Game (Solution word) (freshState word) (Guesses [])
  where freshState w = GameState (map (\_ -> Empty) w)

guess :: Char -> Game -> Game
guess c (Game s@(Solution s') (GameState letters) (Guesses guesses)) = Game s newState newGuesses
  where newState = GameState $ reveal c s' letters
        newGuesses = Guesses (c:guesses)

reveal :: Char -> [Char] -> [Letter] -> [Letter]
reveal c solution letters = map resolve . zip solution $ letters
  where resolve (char, a@(Solved _)) = a
        resolve (char, Empty)
          | char == c = Solved c
          | otherwise = Empty

runGame :: Game -> IO ()
runGame g = forever $ do
  -- gameOver g
  -- gameWin g
  putStrLn (show g) >> putStrLn "Guess a letter: "

  c <- getLine
  case c of
    [c'] -> runGame $ guess c' g
    _   -> putStrLn "Your guess must be a single character"