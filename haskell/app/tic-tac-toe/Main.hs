module Main where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust)
import System.IO (hFlush, stdout)

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
  putStrLn $ show rowNum ++ " | " ++ intercalate " | " (map cellToString cells) ++ " |"

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
runGameIO = gameLoopIO initialGameState
  where
    gameLoopIO :: GameState -> IO ()
    gameLoopIO gameState = do
      let currentBoard = board gameState
          currentPlayer' = currentPlayer gameState
          isOver = gameOver gameState
          winner' = winner gameState

      if isOver
        then do
          displayBoard currentBoard
          displayResult winner'
        else do
          displayBoard currentBoard
          displayTurn currentPlayer'

          -- Get user input
          pos <- getUserMove

          -- Run pure game logic
          let (result, newState) = runState (gameEngine (MakeMove pos)) gameState

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
