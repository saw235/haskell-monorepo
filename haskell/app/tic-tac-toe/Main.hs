{-|
Module      : Main
Description : Tic-Tac-Toe Game - Console Interface
Copyright   : (c) 2024
License     : MIT
Maintainer  : 
Stability   : experimental
Portability : POSIX

This module provides the main entry point for the Tic-Tac-Toe game console interface.
It handles all user interaction, display logic, and coordinates with the pure game logic
defined in the GameLogic module.

The architecture separates concerns by keeping all IO operations in this module while
delegating pure game logic to the GameLogic module. This ensures testability and
maintainability of the codebase.

Key Features:
- Interactive console-based game interface
- Input validation and error handling
- Clean separation of IO and pure logic
- User-friendly board display
- Comprehensive game state feedback

Usage:
    @
    main :: IO ()
    main = do
      putStrLn "Welcome to Tic-Tac-Toe!"
      runGameIO
    @
-}

module Main where

import Control.Monad (unless, when)
import Control.Monad.State (runState)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.IO (hFlush, stdout)

-- Import game logic from GameLogic module
import qualified GameLogic as Game

-- | Type aliases for convenience and readability
type Player = Game.Player
type Cell = Game.Cell
type Board = Game.Board
type Position = Game.Position
type GameState = Game.GameState
type GameAction = Game.GameAction

{-|
Display the current game board in a formatted ASCII representation.

The board is displayed with row and column numbers for easy reference:
    1   2   3
  +---+---+---+
1 | X | O |   |
  +---+---+---+
2 |   | X |   |
  +---+---+---+
3 | O |   | X |
  +---+---+---+

@param board The current game board to display
@return IO action that prints the formatted board
-}
displayBoard :: Board -> IO ()
displayBoard board = do
  putStrLn "\n"
  putStrLn "    1   2   3"
  putStrLn "  +---+---+---+"
  mapM_ displayRow (zip [1 ..] board)
  putStrLn "  +---+---+---+"
  putStrLn ""

{-|
Display a single row of the game board with row number and cell separators.

@param rowNum The row number (1-based for display)
@param cells The list of cells in this row
@return IO action that prints the formatted row
-}
displayRow :: (Int, [Cell]) -> IO ()
displayRow (rowNum, cells) = do
  putStrLn $ show rowNum ++ " | " ++ intercalate " | " (map Game.cellToString cells) ++ " |"

{-|
Get a valid move from the user through console input.

This function handles user input validation and provides helpful error messages
for invalid input. It continues to prompt until valid input is received.

@return IO action that returns a valid Position
-}
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

{-|
Parse a position from string input.

Converts user-friendly 1-based coordinates to internal 0-based coordinates.
Validates that coordinates are within the valid range (1-3).

@param input The string input from the user
@return Maybe Position - Just position if valid, Nothing if invalid
-}
parsePosition :: String -> Maybe Position
parsePosition input = case words input of
  [rowStr, colStr] -> do
    row <- readMaybe rowStr
    col <- readMaybe colStr
    if row >= 1 && row <= 3 && col >= 1 && col <= 3
      then Just (row - 1, col - 1) -- Convert to 0-based indexing
      else Nothing
  _ -> Nothing

{-|
Safe read function that returns Maybe instead of throwing exceptions.

This is used for parsing user input without causing runtime errors.

@param s The string to parse
@return Maybe a - Just value if parsing succeeds, Nothing if it fails
-}
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

{-|
Display the game result when the game ends.

Shows appropriate messages for wins and ties with celebratory emojis.

@param winner Maybe Player - Just player if there's a winner, Nothing for tie
@return IO action that displays the result
-}
displayResult :: Maybe Player -> IO ()
displayResult winner = case winner of
  Just player -> putStrLn $ "\n🎉 Player " ++ show player ++ " wins! 🎉"
  Nothing -> putStrLn "\n🤝 It's a tie! 🤝"

{-|
Display whose turn it is currently.

@param player The current player (X or O)
@return IO action that displays the current player's turn
-}
displayTurn :: Player -> IO ()
displayTurn player = putStrLn $ "\nPlayer " ++ show player ++ "'s turn"

{-|
Main game loop that handles the interactive console game.

This function orchestrates the entire game flow:
1. Displays the current board state
2. Shows whose turn it is
3. Gets user input for the next move
4. Processes the move through the pure game logic
5. Handles errors and continues the loop

The function uses the State monad from GameLogic to maintain pure game state
while keeping all IO operations in this module.

@return IO action that runs the complete game
-}
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

{-|
Main entry point for the Tic-Tac-Toe console application.

This function:
1. Displays welcome message and game instructions
2. Initializes the game with the initial state
3. Runs the interactive game loop
4. Displays a farewell message when the game ends

The game uses a clean architecture where:
- IO operations are contained in this module
- Pure game logic is in the GameLogic module
- State management is handled through the State monad

@return IO action that runs the complete application
-}
main :: IO ()
main = do
  putStrLn "Welcome to Tic-Tac-Toe!"
  putStrLn "Players take turns placing X and O on the board."
  putStrLn "The first player to get 3 in a row (horizontally, vertically, or diagonally) wins!"

  -- Run the game with separated IO
  runGameIO

  putStrLn "\nThanks for playing!"
