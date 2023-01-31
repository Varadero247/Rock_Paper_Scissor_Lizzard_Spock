module Rock_Paper_Scissor_Lizzard_Spock where

import System.Random
import Control.Monad (when)
import Text.Read (readMaybe)

-- Define the possible moves
data Move = Rock | Paper | Scissors | Spock | Lizard
  deriving (Eq, Show)

-- Define the rules of the game using a map
beats :: Move -> Move -> Bool
beats playerMove computerMove =
  let rules = [(Rock, [Scissors, Lizard]),
              (Paper, [Rock, Spock]),
              (Scissors, [Paper, Lizard]),
              (Spock, [Rock, Scissors]),
              (Lizard, [Paper, Spock])]
      result = lookup playerMove rules
  in case result of
      Just xs -> computerMove `elem` xs
      Nothing -> False

-- Prompt the user for their move and get their input
getPlayerMove :: IO Move
getPlayerMove = do
  let validMoves = ["rock", "paper", "scissors", "spock", "lizard"]
  putStrLn "Please enter your move (rock, paper, scissors, spock, or lizard):"
  moveStr <- getLine
  case lookup moveStr [("rock", Rock),
                      ("paper", Paper),
                      ("scissors", Scissors),
                      ("spock", Spock),
                      ("lizard", Lizard)] of
    Just move -> return move
    Nothing -> do
      putStrLn "Invalid move. Please try again."
      getPlayerMove

-- Generate a random move for the computer
getComputerMove :: IO Move
getComputerMove = do
  index <- randomRIO (0, 4) :: IO Int
  return $ case index of
    0 -> Rock
    1 -> Paper
    2 -> Scissors
    3 -> Spock
    4 -> Lizard

-- Play a single round of the game
playRound :: IO ()
playRound = do
  -- Get the player's move
  playerMove <- getPlayerMove

  -- Get the computer's move
  computerMove <- getComputerMove

  -- Display the moves
  putStrLn $ "You played " ++ show playerMove ++ " and the computer played " ++ show computerMove

  -- Determine the winner
  let result
        | playerMove == computerMove = "It's a tie!"
        | beats playerMove computerMove = "You win!"
        | otherwise = "The computer wins!"

  -- Display the result
  putStrLn result

-- Play the game until the player decides to stop
playGame :: IO ()
playGame = do
  playRound
  putStrLn "Do you want to play again (y/n)?"
  input <- getLine
  when (input == "y") playGame

-- Main function
main :: IO ()
main = do
  putStrLn "Welcome to Rock-Paper-Scissors-Spock!"
  playGame
