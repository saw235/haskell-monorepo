module Main where

import Control.Monad (unless, when)
import Control.Monad.State (runState)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)

-- Import game logic from GameLogic module
import qualified GameLogic as Game

-- Type aliases for convenience
type Player = Game.Player
type Cell = Game.Cell
type Board = Game.Board
type Position = Game.Position
type GameState = Game.GameState
type GameAction = Game.GameAction

-- Display the board
displayBoard :: Board -> IO ()
displayBoard board = do
  putStrLn "\n"
  putStrLn "    1   2   3"
  putStrLn "  +---+---+---+"
  mapM_ displayRow (zip [1 ..] board)
  putStrLn "  +---+---+---+"
  putStrLn ""

-- Display a single row
displayRow :: (Int, [Cell]) -> IO ()
displayRow (rowNum, cells) = do
  putStrLn $ show rowNum ++ " | " ++ intercalate " | " (map Game.cellToString cells) ++ " |"

-- IO functions (separate from game logic)
getUserMove :: IO Position
getUserMove = do
  putStrLn "Enter row (1-3) and column (1-3) separated by space (e.g., '2 3'):"
  putStr "> "
  hFlush stdout
  input <- getLine
  case parsePosition input of
    Just pos -> return pos
    Nothing -> do
      putStrLn "Invalid input! Please enter row and column as numbers 1-3."
      getUserMove

-- Parse position from string input
parsePosition :: String -> Maybe Position
parsePosition input = case words input of
  [rowStr, colStr] -> do
    row <- readMaybe rowStr
    col <- readMaybe colStr
    if row >= 1 && row <= 3 && col >= 1 && col <= 3
      then Just (row - 1, col - 1) -- Convert to 0-based indexing
      else Nothing
  _ -> Nothing

-- Safe read function
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- Display game result
displayResult :: Maybe Player -> IO ()
displayResult winner = case winner of
  Just player -> putStrLn $ "\n🎉 Player " ++ show player ++ " wins! 🎉"
  Nothing -> putStrLn "\n🤝 It's a tie! 🤝"

-- Display current player's turn
displayTurn :: Player -> IO ()
displayTurn player = putStrLn $ "\nPlayer " ++ show player ++ "'s turn"

-- IO wrapper for the game
runGameIO :: IO ()
runGameIO = gameLoopIO Game.initialGameState
  where
    gameLoopIO :: GameState -> IO ()
    gameLoopIO gameState = do
      let currentBoard = Game.board gameState
          currentPlayer' = Game.currentPlayer gameState
          isOver = Game.gameOver gameState
          winner' = Game.winner gameState

      if isOver
        then do
          displayBoard currentBoard
          displayResult winner'
        else do
          displayBoard currentBoard
          displayTurn currentPlayer'

          -- Get user input
          pos <- getUserMove

          -- Run pure game logic from GameLogic module
          let (result, newState) = runState (Game.gameEngine (Game.MakeMove pos)) gameState

          case result of
            Left errorMsg -> do
              putStrLn $ "Error: " ++ errorMsg
              gameLoopIO gameState -- Try again with same state
            Right _ -> do
              gameLoopIO newState -- Continue with new state

-- Main function
main :: IO ()
main = do
  putStrLn "Welcome to Tic-Tac-Toe!"
  putStrLn "Players take turns placing X and O on the board."
  putStrLn "The first player to get 3 in a row (horizontally, vertically, or diagonally) wins!"

  -- Run the game with separated IO
  runGameIO

  putStrLn "\nThanks for playing!"
