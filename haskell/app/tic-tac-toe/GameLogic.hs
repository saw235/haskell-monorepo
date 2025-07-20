-- |
-- Module      : GameLogic
-- Description : Pure Tic-Tac-Toe Game Logic
-- Copyright   : (c) 2024
-- License     : MIT
-- Maintainer  :
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains all the pure game logic for the Tic-Tac-Toe game.
-- It provides a clean separation between pure game logic and IO operations,
-- making the code testable and maintainable.
--
-- The module uses the State monad for managing game state transitions,
-- ensuring that all game operations are pure and predictable.
--
-- Key Features:
-- - Pure game logic with no IO dependencies
-- - State monad for game state management
-- - Comprehensive win condition checking
-- - Move validation and board updates
-- - Type-safe game actions and state
--
-- Architecture:
-- - GameState: Contains board, current player, game status
-- - GameAction: Represents possible game actions (currently MakeMove)
-- - State monad: Manages state transitions atomically
-- - Pure functions: All game logic functions are pure
--
-- Usage:
--     @
--     -- Initialize a new game
--     let initialState = initialGameState
--
--     -- Make a move
--     let (result, newState) = runState (gameEngine (MakeMove (1, 1))) initialState
--
--     -- Check game status
--     let isOver = gameOver newState
--     let winner = winner newState
--     @
module GameLogic where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust)

-- =============================================================================
-- GAME TYPES AND DATA STRUCTURES
-- =============================================================================

-- |
-- Represents the two players in the game.
-- X always goes first in a new game.
data Player = X | O deriving (Eq, Show)

-- |
-- Represents the state of a single cell on the board.
-- A cell can be either empty or filled by a specific player.
data Cell = Empty | Filled Player deriving (Eq, Show)

-- |
-- The game board is a 3x3 grid of cells.
-- Represented as a list of rows, where each row is a list of cells.
-- Indexing is 0-based: board[row][col]
type Board = [[Cell]]

-- |
-- Represents a position on the board as (row, col) coordinates.
-- Both row and col are 0-based indices (0-2).
type Position = (Int, Int)

-- |
-- Complete game state containing all information needed to represent
-- the current state of the game.
--
-- Fields:
-- - board: Current state of the 3x3 game board
-- - currentPlayer: Player whose turn it is (X or O)
-- - gameOver: Whether the game has ended
-- - winner: Player who won (if any), Nothing for tie or ongoing game
data GameState = GameState
  { board :: Board,
    currentPlayer :: Player,
    gameOver :: Bool,
    winner :: Maybe Player
  }
  deriving (Show, Eq)

-- |
-- Pure game monad type using State for managing game state.
-- All game operations are pure and can be composed safely.
type Game = State GameState

-- |
-- Represents actions that can be performed in the game.
-- Currently only supports making moves, but can be extended
-- for other game actions like undo, reset, etc.
data GameAction = MakeMove Position deriving (Show)

-- =============================================================================
-- INITIALIZATION FUNCTIONS
-- =============================================================================

-- |
-- Creates an empty 3x3 board with all cells set to Empty.
--
-- @return Board - A 3x3 grid of empty cells
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

-- |
-- Creates the initial game state for a new game.
--
-- The initial state has:
-- - Empty 3x3 board
-- - Player X as the first player
-- - Game not over
-- - No winner
--
-- @return GameState - The initial state for a new game
initialGameState :: GameState
initialGameState =
  GameState
    { board = emptyBoard,
      currentPlayer = X,
      gameOver = False,
      winner = Nothing
    }

-- =============================================================================
-- UTILITY FUNCTIONS
-- =============================================================================

-- |
-- Converts a cell to its string representation for display.
--
-- @param cell The cell to convert
-- @return String representation: " " for empty, "X" for X, "O" for O
cellToString :: Cell -> String
cellToString Empty = " "
cellToString (Filled X) = "X"
cellToString (Filled O) = "O"

-- |
-- Gets all valid moves (empty positions) on the current board.
--
-- @param board The current game board
-- @return [Position] - List of all empty positions where moves can be made
validMoves :: Board -> [Position]
validMoves board = [(row, col) | row <- [0 .. 2], col <- [0 .. 2], board !! row !! col == Empty]

-- |
-- Updates the board with a new move at the specified position.
--
-- This function creates a new board with the move applied, leaving the original
-- board unchanged (immutable update).
--
-- @param board The current board
-- @param position The position to place the move (row, col)
-- @param player The player making the move
-- @return Board - New board with the move applied
updateBoard :: Board -> Position -> Player -> Board
updateBoard board (row, col) player =
  take row board
    ++ [take col (board !! row) ++ [Filled player] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

-- =============================================================================
-- GAME STATUS FUNCTIONS
-- =============================================================================

-- |
-- Checks if the game is over (either someone won or it's a tie).
--
-- @param board The current game board
-- @return Bool - True if game is over, False if game continues
isGameOver :: Board -> Bool
isGameOver board = isJust (getWinner board) || all (all (/= Empty)) board

-- |
-- Determines if there's a winner on the current board.
--
-- Checks all possible winning combinations:
-- - Three in a row (horizontal)
-- - Three in a column (vertical)
-- - Three in a diagonal (both directions)
--
-- @param board The current game board
-- @return Maybe Player - Just player if there's a winner, Nothing if no winner
getWinner :: Board -> Maybe Player
getWinner board
  | hasWinningLine board X = Just X
  | hasWinningLine board O = Just O
  | otherwise = Nothing

-- |
-- Checks if a specific player has a winning line on the board.
--
-- A winning line consists of three of the player's marks in a row:
-- - Horizontally across any row
-- - Vertically down any column
-- - Diagonally from corner to corner (both directions)
--
-- @param board The current game board
-- @param player The player to check for a win
-- @return Bool - True if player has a winning line, False otherwise
hasWinningLine :: Board -> Player -> Bool
hasWinningLine board player =
  any (all (== Filled player)) board
    || any (all (== Filled player)) (transpose board) -- Rows
    || all (== Filled player) [board !! i !! i | i <- [0 .. 2]] -- Columns
    || all (== Filled player) [board !! i !! (2 - i) | i <- [0 .. 2]] -- Diagonal
    -- Anti-diagonal

-- =============================================================================
-- STATE MONAD ACCESSORS
-- =============================================================================

-- |
-- Gets the current board state from the game monad.
--
-- @return Game Board - Current board state
getBoardState :: Game Board
getBoardState = board <$> get

-- |
-- Gets the current player from the game monad.
--
-- @return Game Player - Current player whose turn it is
getCurrentPlayerState :: Game Player
getCurrentPlayerState = currentPlayer <$> get

-- |
-- Checks if the game is over in the current state.
--
-- @return Game Bool - Whether the game has ended
isGameOverState :: Game Bool
isGameOverState = gameOver <$> get

-- |
-- Gets the winner from the current state.
--
-- @return Game (Maybe Player) - Winner if game is over, Nothing otherwise
getWinnerState :: Game (Maybe Player)
getWinnerState = winner <$> get

-- =============================================================================
-- GAME LOGIC FUNCTIONS
-- =============================================================================

-- |
-- Attempts to make a move at the specified position.
--
-- This function validates the move and updates the game state if valid.
-- The move is only applied if:
-- - Position is within bounds (0-2 for both row and col)
-- - Position is currently empty
-- - Game is not already over
--
-- @param position The position to place the move (row, col)
-- @return Game Bool - True if move was successful, False if invalid
makeMoveState :: Position -> Game Bool
makeMoveState (row, col) = do
  gameState <- get
  let currentBoard = board gameState
      currentPlayer' = currentPlayer gameState

  -- Check if move is valid
  if row < 0 || row > 2 || col < 0 || col > 2 || currentBoard !! row !! col /= Empty
    then return False
    else do
      -- Update the state
      let newBoard = updateBoard currentBoard (row, col) currentPlayer'
          newPlayer = if currentPlayer' == X then O else X
          newGameOver = isGameOver newBoard
          newWinner = getWinner newBoard

      put $
        gameState
          { board = newBoard,
            currentPlayer = newPlayer,
            gameOver = newGameOver,
            winner = newWinner
          }
      return True

-- |
-- Switches the current player from X to O or vice versa.
--
-- This is a utility function for manually switching turns,
-- though it's typically handled automatically by makeMoveState.
--
-- @return Game () - Action that switches the current player
switchPlayer :: Game ()
switchPlayer = modify $ \state -> state {currentPlayer = if currentPlayer state == X then O else X}

-- |
-- Main game engine that processes game actions.
--
-- This is the primary interface for making moves in the game.
-- It takes a GameAction and returns either a success message or an error.
--
-- @param action The game action to perform
-- @return Game (Either String String) - Right with success message or Left with error
gameEngine :: GameAction -> Game (Either String String)
gameEngine (MakeMove pos) = do
  success <- makeMoveState pos
  if success
    then return $ Right "Move successful"
    else return $ Left "Invalid move"
