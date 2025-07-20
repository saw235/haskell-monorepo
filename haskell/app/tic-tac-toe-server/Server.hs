{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

-- STM (Software Transactional Memory) for atomic state management
-- Provides transactional state updates with automatic rollback on failure
-- Replaces IORef for better concurrency safety
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)
-- Import game types and logic from GameLogic
import qualified GameLogic as Game
import Network.HTTP.Types (methodGet, methodPost, status200, status400, status404)
import Network.Wai (Application, Response, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setPort)
import Network.Wai.Internal (getRequestBodyChunk)
import System.IO (hPutStrLn, stderr, stdout)

-- =============================================================================
-- LOGGING SYSTEM
-- =============================================================================
-- Debug logging functions for tracking game events and server behavior
-- All logging functions are IO-based and thread-safe

logDebug :: String -> IO ()
logDebug msg = hPutStrLn stderr $ "[DEBUG] " ++ msg

logInfo :: String -> IO ()
logInfo msg = hPutStrLn stdout $ "[INFO] " ++ msg

logError :: String -> IO ()
logError msg = hPutStrLn stderr $ "[ERROR] " ++ msg

-- Log game state changes with detailed board visualization
-- Used for debugging state transitions and game flow
logGameState :: String -> Game.GameState -> IO ()
logGameState context state = do
  logDebug $
    context
      ++ " - Board: "
      ++ showBoard (Game.board state)
      ++ ", Current Player: "
      ++ show (Game.currentPlayer state)
      ++ ", Game Over: "
      ++ show (Game.gameOver state)
      ++ ", Winner: "
      ++ maybe "None" show (Game.winner state)
  where
    showBoard board = intercalate "|" $ map showRow board
    showRow row = "[" ++ intercalate "," (map showCell row) ++ "]"
    showCell Game.Empty = " "
    showCell (Game.Filled Game.X) = "X"
    showCell (Game.Filled Game.O) = "O"

-- Log valid moves for debugging move validation
logValidMoves :: [Game.Position] -> IO ()
logValidMoves moves = do
  logDebug $ "Valid moves: " ++ show moves

-- =============================================================================
-- API TYPES AND JSON SERIALIZATION
-- =============================================================================
-- JSON types for HTTP API communication between client and server
-- Uses Aeson for automatic JSON serialization/deserialization

data GameRequest = GameRequest
  { action :: String, -- Game action: "new_game", "make_move", "get_state"
    position :: Maybe Game.Position -- Optional position for moves
  }
  deriving (Show, Eq, Generic)

instance FromJSON GameRequest

instance ToJSON GameRequest

data GameResponse = GameResponse
  { board :: [[String]], -- 3x3 board representation for JSON
    currentPlayer :: String, -- "X" or "O"
    gameOver :: Bool, -- Whether the game has ended
    winner :: Maybe String, -- Winner if game is over
    message :: String, -- Human-readable status message
    validMoves :: [Game.Position] -- Available moves for client validation
  }
  deriving (Show, Eq, Generic)

instance ToJSON GameResponse
instance FromJSON GameResponse

-- =============================================================================
-- BOARD CONVERSION UTILITIES
-- =============================================================================
-- Convert between internal Game.Board type and JSON-friendly string representation
-- Ensures consistent serialization across the API

-- Convert internal board representation to JSON-friendly strings
boardToJson :: Game.Board -> [[String]]
boardToJson board = map (map cellToString) board
  where
    cellToString Game.Empty = " "
    cellToString (Game.Filled Game.X) = "X"
    cellToString (Game.Filled Game.O) = "O"

-- Convert JSON strings back to internal board representation
jsonToBoard :: [[String]] -> Game.Board
jsonToBoard jsonBoard = map (map stringToCell) jsonBoard
  where
    stringToCell " " = Game.Empty
    stringToCell "X" = Game.Filled Game.X
    stringToCell "O" = Game.Filled Game.O
    stringToCell _ = Game.Empty

-- =============================================================================
-- SERVER STATE MANAGEMENT
-- =============================================================================
-- Server state wrapper around the game state
-- This is the shared state that persists across HTTP requests

data ServerState = ServerState
  { gameState :: Game.GameState -- Current game state from GameLogic
  }
  deriving (Show, Eq)

-- Initial server state with empty game
initialServerState :: ServerState
initialServerState = ServerState {gameState = Game.initialGameState}

-- =============================================================================
-- GAME ACTION PROCESSING - STM VERSION
-- =============================================================================
-- Pure STM version for atomic state transactions
-- This version contains no IO operations and can be used within STM transactions
-- Provides automatic rollback if any part of the transaction fails

processGameActionSTM :: GameRequest -> ServerState -> STM (GameResponse, ServerState)
processGameActionSTM request serverState = case action request of
  "new_game" -> do
    let newServerState = ServerState {gameState = Game.initialGameState}
    return (createNewGameResponse, newServerState)
  "make_move" -> case position request of
    Just pos -> makeMoveResponseSTM pos serverState
    Nothing -> return (errorResponse "Position required for make_move", serverState)
  "get_state" -> return (getStateResponse serverState, serverState)
  _ -> return (errorResponse "Unknown action", serverState)
  where
    createNewGameResponse =
      GameResponse
        { board = boardToJson (Game.board Game.initialGameState),
          currentPlayer = show (Game.currentPlayer Game.initialGameState),
          gameOver = Game.gameOver Game.initialGameState,
          winner = fmap show (Game.winner Game.initialGameState),
          message = "New game started",
          validMoves = Game.validMoves (Game.board Game.initialGameState)
        }

-- =============================================================================
-- GAME ACTION PROCESSING - IO VERSION (LEGACY)
-- =============================================================================
-- IO version with comprehensive logging for debugging
-- This version is kept for reference and could be used in single-threaded scenarios
-- Note: The STM version is preferred for production use

processGameAction :: GameRequest -> ServerState -> IO (GameResponse, ServerState)
processGameAction request serverState = do
  logDebug $ "Processing game action: " ++ action request
  case action request of
    "new_game" -> do
      logInfo "Starting new game"
      let newServerState = ServerState {gameState = Game.initialGameState}
      logGameState "New game state" (gameState newServerState)
      logValidMoves (Game.validMoves (Game.board Game.initialGameState))
      return (createNewGameResponse, newServerState)
    "make_move" -> case position request of
      Just pos -> do
        logDebug $ "Making move at position: " ++ show pos
        makeMoveResponse pos serverState
      Nothing -> do
        logError "Position required for make_move but not provided"
        return (errorResponse "Position required for make_move", serverState)
    "get_state" -> do
      logDebug "Getting current game state"
      logGameState "Current state" (gameState serverState)
      return (getStateResponse serverState, serverState)
    _ -> do
      logError $ "Unknown action: " ++ action request
      return (errorResponse "Unknown action", serverState)
  where
    createNewGameResponse =
      GameResponse
        { board = boardToJson (Game.board Game.initialGameState),
          currentPlayer = show (Game.currentPlayer Game.initialGameState),
          gameOver = Game.gameOver Game.initialGameState,
          winner = fmap show (Game.winner Game.initialGameState),
          message = "New game started",
          validMoves = Game.validMoves (Game.board Game.initialGameState)
        }

-- Make move response (STM version - pure logic)
-- Handles move validation and state updates within STM transactions
-- Returns error response if move is invalid, otherwise updates state
makeMoveResponseSTM :: Game.Position -> ServerState -> STM (GameResponse, ServerState)
makeMoveResponseSTM pos serverState = do
  let (result, newGameState) = runState (Game.gameEngine (Game.MakeMove pos)) (gameState serverState)
      newServerState = ServerState {gameState = newGameState}

  case result of
    Left errorMsg -> return (errorResponse errorMsg, serverState)
    Right _ -> return (getStateResponse newServerState, newServerState)

-- Make move response (IO version - with logging)
-- Legacy version with detailed logging for debugging
-- Used for development and debugging purposes
makeMoveResponse :: Game.Position -> ServerState -> IO (GameResponse, ServerState)
makeMoveResponse pos serverState = do
  logDebug $ "Attempting move at position: " ++ show pos
  logGameState "Before move" (gameState serverState)

  let (result, newGameState) = runState (Game.gameEngine (Game.MakeMove pos)) (gameState serverState)
      newServerState = ServerState {gameState = newGameState}

  case result of
    Left errorMsg -> do
      logError $ "Move failed: " ++ errorMsg
      return (errorResponse errorMsg, serverState)
    Right _ -> do
      logInfo $ "Move successful at position: " ++ show pos
      logGameState "After move" newGameState
      logValidMoves (Game.validMoves (Game.board newGameState))

      -- Log game end conditions
      when (Game.gameOver newGameState) $ do
        case Game.winner newGameState of
          Just winner -> logInfo $ "Game over! Winner: " ++ show winner
          Nothing -> logInfo "Game over! It's a draw"

      return (getStateResponse newServerState, newServerState)

-- =============================================================================
-- RESPONSE GENERATION
-- =============================================================================
-- Pure functions for generating API responses

-- Generate response for current game state
getStateResponse :: ServerState -> GameResponse
getStateResponse serverState =
  let state = gameState serverState
   in GameResponse
        { board = boardToJson (Game.board state),
          currentPlayer = show (Game.currentPlayer state),
          gameOver = Game.gameOver state,
          winner = fmap show (Game.winner state),
          message = "Current game state",
          validMoves = Game.validMoves (Game.board state)
        }

-- Generate error response with empty board
errorResponse :: String -> GameResponse
errorResponse msg =
  GameResponse
    { board = boardToJson Game.emptyBoard,
      currentPlayer = "X",
      gameOver = False,
      winner = Nothing,
      message = msg,
      validMoves = []
    }

-- =============================================================================
-- WAI APPLICATION - STM VERSION
-- =============================================================================
-- Main HTTP application using STM for atomic state management
--
-- ARCHITECTURAL DECISIONS:
-- 1. Uses TVar instead of IORef for thread-safe state management
-- 2. Wraps state operations in atomically for transactional safety
-- 3. Separates pure STM logic from IO logging operations
-- 4. Provides automatic rollback on transaction failure
-- 5. Warp automatically handles multi-threading and request distribution
--
-- CONCURRENCY BENEFITS:
-- - Multiple threads can safely access shared state
-- - Atomic transactions prevent race conditions
-- - Automatic deadlock detection and resolution
-- - No manual locking required
-- - Warp's thread pool handles concurrent requests automatically

app :: TVar ServerState -> Application
app stateVar request respond = do
  logDebug $ "Received " ++ show (requestMethod request) ++ " request"

  case (requestMethod request, BS.unpack $ requestMethod request) of
    (method, "POST") | method == methodPost -> do
      body <- getRequestBodyChunk request
      logDebug $ "POST body: " ++ show body

      case decode (LBS.fromStrict body) of
        Just gameRequest -> do
          logDebug $ "Parsed request: " ++ show gameRequest
          -- STM transaction: atomic read-modify-write operation
          result <- atomically $ do
            currentState <- readTVar stateVar
            (response, newServerState) <- processGameActionSTM gameRequest currentState
            writeTVar stateVar newServerState
            return response
          logDebug $ "Sending response: " ++ show result
          respond $ responseLBS status200 [("Content-Type", "application/json")] (encode result)
        Nothing -> do
          logError "Failed to parse JSON request"
          let errorResp = errorResponse "Invalid JSON"
          respond $ responseLBS status400 [("Content-Type", "application/json")] (encode errorResp)
    (method, "GET") | method == methodGet -> do
      logDebug "Handling GET request for current state"
      -- Atomic read operation
      currentState <- atomically $ readTVar stateVar
      let response = getStateResponse currentState
      logGameState "GET request state" (gameState currentState)
      respond $ responseLBS status200 [("Content-Type", "application/json")] (encode response)
    _ -> do
      logError $ "Method not allowed: " ++ show (requestMethod request)
      let errorResp = errorResponse "Method not allowed"
      respond $ responseLBS status404 [("Content-Type", "application/json")] (encode errorResp)

-- =============================================================================
-- CORS MIDDLEWARE
-- =============================================================================
-- Cross-Origin Resource Sharing middleware for web client compatibility
-- Allows browser-based clients to make requests to the server

corsMiddleware :: Application -> Application
corsMiddleware app request respond = do
  app request $ \response -> do
    let headers =
          [ ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS"),
            ("Access-Control-Allow-Headers", "Content-Type")
          ]
    respond response

-- =============================================================================
-- SERVER INITIALIZATION
-- =============================================================================
-- Server startup with STM state management and multi-threaded Warp
--
-- ARCHITECTURAL CHANGES FROM IOREF VERSION:
-- 1. Uses TVar instead of IORef for thread-safe state
-- 2. Warp automatically handles multi-threading (no manual configuration needed)
-- 3. Uses runSettings for explicit configuration
-- 4. STM transactions provide atomic state updates
--
-- BENEFITS:
-- - Concurrent request handling without race conditions
-- - Automatic transaction rollback on failure
-- - Better performance under load
-- - No manual synchronization required
-- - Warp's built-in thread pool handles concurrency automatically

startServer :: Int -> IO ()
startServer port = do
  logInfo $ "Starting Tic-Tac-Toe server on port " ++ show port
  logInfo "Debug logging enabled - all game events will be logged"
  logInfo "Using STM for state management with Warp's automatic threading"
  logGameState "Initial server state" (gameState initialServerState)

  -- Initialize STM state variable
  stateVar <- atomically $ newTVar initialServerState

  -- Configure Warp with custom settings
  -- Note: Warp automatically handles multi-threading - no setThreads needed
  let settings = setPort port defaultSettings
  runSettings settings (corsMiddleware (app stateVar))
