module Lib (randomWord, freshPuzzle, runGame) where

  import Control.Monad (forever)
  import Data.Maybe (isJust)
  import Data.List (intersperse)
  import System.Exit (exitSuccess)
  import System.Random (Random, randomRIO)
  
  type WordList = [String]
  
  allWords :: IO WordList
  allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)
  
  minWordLength :: Int
  minWordLength = 5
  
  maxWordLength :: Int
  maxWordLength = 9
  
  gameWords :: IO WordList
  gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength word =
            let l = length word 
            in l >= minWordLength &&
               l < maxWordLength
  
  randomIndex :: (Random a, Num a) => a -> a -> IO a
  randomIndex lower upper = randomRIO (lower, upper - 1)
  
  randomWord :: IO String
  randomWord = do
    gw <- gameWords
    i <- randomIndex 0 (length gw - 1)
    return $ gw !! i
  
  data Puzzle = Puzzle String [Maybe Char] [Char]
  
  instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
      (intersperse ' ' $
       fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed
  
  renderPuzzleChar :: Maybe Char -> Char
  renderPuzzleChar Nothing = '_'
  renderPuzzleChar (Just c) = c
  
  freshPuzzle :: String -> Puzzle
  freshPuzzle word = Puzzle word [] []
  
  charInWord :: Puzzle -> Char -> Bool
  charInWord (Puzzle word _ _) guess = elem guess word
  
  alreadyGuessed :: Puzzle -> Char -> Bool
  alreadyGuessed (Puzzle _ _ guesses) guess = elem guess guesses
  
  fillInCharacter :: Puzzle -> Char -> Puzzle
  fillInCharacter (Puzzle word discovered guesses) guess =
    Puzzle word newDiscovered (guess:guesses)
    where newDiscovered = (map snd) . (replaceChar guess) $ zip word discovered
  
  replaceChar :: Char -> [(Char, Maybe Char)] -> [(Char, Maybe Char)]
  replaceChar c lst = fmap (replaceChar' c) lst
  
  replaceChar' :: Char -> (Char, Maybe Char) -> (Char, Maybe Char)
  replaceChar' _ (w, Just c) = (w, Just c)
  replaceChar' guess (w, Nothing)
    | guess == w = (w, Just guess)
    | otherwise = (w, Nothing)
  
  handleGuess :: Puzzle -> Char -> IO Puzzle
  handleGuess p guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord p guess, alreadyGuessed p guess) of
      (_, True)  -> putStrLn "You already guessed that character" >> return p
      (True, _)  -> putStrLn "Great job! This character was in the word." >> return (fillInCharacter p guess)
      (False, _) -> putStrLn "Sorry, no dice." >> return (fillInCharacter p guess)
  
  gameOver :: Puzzle -> IO ()
  gameOver (Puzzle word _ guessed)
    | length guessed > 7 = putStrLn "You lose!" >> putStrLn ("The word was: " ++ word) >> exitSuccess
    | otherwise = return ()
  
  gameWin :: Puzzle -> IO ()
  gameWin (Puzzle _ discovered _)
    | all isJust discovered = putStrLn "You win!" >> exitSuccess
    | otherwise = return ()
  
  runGame :: Puzzle -> IO ()
  runGame p = forever $ do
    gameOver p
    gameWin p
    putStrLn ("Current puzzle is: " ++ show p) >> putStr "Guess a letter: "
  
    guess <- getLine
    case guess of
      [c] -> handleGuess p c >>= runGame
      _   -> putStrLn "Your guess must be a single character"
  