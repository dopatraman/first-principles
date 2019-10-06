module Game (Letter(..)
           , GameState(..)
           , Guesses(..)
           , Game(..)) where

import Data.List (intersperse)
import Data.Char (toUpper)
import Control.Monad (forever)
import System.Exit (exitSuccess)

data Letter = Empty Char | Solved Char
newtype GameState = GameState [Letter]
newtype Guesses = Guesses [Char]

data Game = Game GameState Guesses

instance Show GameState where
  show (GameState []) = ""
  show (GameState ((Empty _):letters)) = '_':(show (GameState letters))
  show (GameState ((Solved c):letters)) = c:(show (GameState letters))

instance Show Guesses where
  show (Guesses []) = ""
  show (Guesses (g:guesses)) = (toUpper g):(show $ Guesses guesses)

instance Show Game where
  show (Game st g) = "Current State: " ++ show st ++ "\nCurrent guesses: " ++ show g

