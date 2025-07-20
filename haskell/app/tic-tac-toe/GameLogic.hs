module GameLogic where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust)

-- Game types
data Player = X | O deriving (Eq, Show)

data Cell = Empty | Filled Player deriving (Eq, Show)

type Board = [[Cell]]

type Position = (Int, Int)

-- Game state
data GameState = GameState
  { board :: Board,
    currentPlayer :: Player,
    gameOver :: Bool,
    winner :: Maybe Player
  }
  deriving (Show)

-- Pure game monad type (no IO)
type Game = State GameState

-- Game actions that can be performed
data GameAction = MakeMove Position deriving (Show)

-- Initialize empty board
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

-- Initialize game state
initialGameState :: GameState
initialGameState =
  GameState
    { board = emptyBoard,
      currentPlayer = X,
      gameOver = False,
      winner = Nothing
    }

-- Convert cell to string for display
cellToString :: Cell -> String
cellToString Empty = " "
cellToString (Filled X) = "X"
cellToString (Filled O) = "O"

-- Get valid moves (empty positions)
validMoves :: Board -> [Position]
validMoves board = [(row, col) | row <- [0 .. 2], col <- [0 .. 2], board !! row !! col == Empty]

-- Update board with a move
updateBoard :: Board -> Position -> Player -> Board
updateBoard board (row, col) player =
  take row board
    ++ [take col (board !! row) ++ [Filled player] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

-- Check if game is over
isGameOver :: Board -> Bool
isGameOver board = isJust (getWinner board) || all (all (/= Empty)) board

-- Get winner (if any)
getWinner :: Board -> Maybe Player
getWinner board
  | hasWinningLine board X = Just X
  | hasWinningLine board O = Just O
  | otherwise = Nothing

-- Check if a player has a winning line
hasWinningLine :: Board -> Player -> Bool
hasWinningLine board player =
  any (all (== Filled player)) board
    || any (all (== Filled player)) (transpose board) -- Rows
    || all (== Filled player) [board !! i !! i | i <- [0 .. 2]] -- Columns
    || all (== Filled player) [board !! i !! (2 - i) | i <- [0 .. 2]] -- Diagonal
    -- Anti-diagonal

-- Pure game logic functions
-- Get current board
getBoardState :: Game Board
getBoardState = board <$> get

-- Get current player
getCurrentPlayerState :: Game Player
getCurrentPlayerState = currentPlayer <$> get

-- Check if game is over
isGameOverState :: Game Bool
isGameOverState = gameOver <$> get

-- Get winner
getWinnerState :: Game (Maybe Player)
getWinnerState = winner <$> get

-- Make a move using pure State monad
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

-- Switch player turn
switchPlayer :: Game ()
switchPlayer = modify $ \state -> state {currentPlayer = if currentPlayer state == X then O else X}

-- Pure game engine - handles move logic
gameEngine :: GameAction -> Game (Either String String)
gameEngine (MakeMove pos) = do
  success <- makeMoveState pos
  if success
    then return $ Right "Move successful"
    else return $ Left "Invalid move" 