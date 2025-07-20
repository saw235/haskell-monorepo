{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import Control.Monad (unless, when)
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, isJust)
import GHC.Generics (Generic)
-- Re-export game types and logic from Main
import qualified Main as Game
import Network.HTTP.Types (methodGet, methodPost, status200, status400, status404)
import Network.Wai (Application, Response, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (getRequestBodyChunk)

-- JSON types for API communication
data GameRequest = GameRequest
  { action :: String,
    position :: Maybe Game.Position
  }
  deriving (Show, Generic)

instance FromJSON GameRequest

instance ToJSON GameRequest

data GameResponse = GameResponse
  { board :: [[String]],
    currentPlayer :: String,
    gameOver :: Bool,
    winner :: Maybe String,
    message :: String,
    validMoves :: [Game.Position]
  }
  deriving (Show, Generic)

instance ToJSON GameResponse

-- Convert board to JSON-friendly format
boardToJson :: Game.Board -> [[String]]
boardToJson board = map (map cellToString) board
  where
    cellToString Game.Empty = " "
    cellToString (Game.Filled Game.X) = "X"
    cellToString (Game.Filled Game.O) = "O"

-- Convert JSON-friendly format back to board
jsonToBoard :: [[String]] -> Game.Board
jsonToBoard jsonBoard = map (map stringToCell) jsonBoard
  where
    stringToCell " " = Game.Empty
    stringToCell "X" = Game.Filled Game.X
    stringToCell "O" = Game.Filled Game.O
    stringToCell _ = Game.Empty

-- Handle game state
data ServerState = ServerState
  { gameState :: Game.GameState
  }

initialServerState :: ServerState
initialServerState = ServerState {gameState = Game.initialGameState}

-- Process game action
processGameAction :: GameRequest -> ServerState -> (GameResponse, ServerState)
processGameAction request serverState = case action request of
  "new_game" -> (createNewGameResponse, newServerState)
  "make_move" -> case position request of
    Just pos -> makeMoveResponse pos serverState
    Nothing -> (errorResponse "Position required for make_move", serverState)
  "get_state" -> (getStateResponse serverState, serverState)
  _ -> (errorResponse "Unknown action", serverState)
  where
    newServerState = ServerState {gameState = Game.initialGameState}
    createNewGameResponse =
      GameResponse
        { board = boardToJson (Game.board Game.initialGameState),
          currentPlayer = show (Game.currentPlayer Game.initialGameState),
          gameOver = Game.gameOver Game.initialGameState,
          winner = fmap show (Game.winner Game.initialGameState),
          message = "New game started",
          validMoves = Game.validMoves (Game.board Game.initialGameState)
        }

makeMoveResponse :: Game.Position -> ServerState -> (GameResponse, ServerState)
makeMoveResponse pos serverState =
  let (result, newGameState) = runState (Game.gameEngine (Game.MakeMove pos)) (gameState serverState)
      newServerState = ServerState {gameState = newGameState}
   in case result of
        Left errorMsg -> (errorResponse errorMsg, serverState)
        Right _ -> (getStateResponse newServerState, newServerState)

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

-- WAI Application
app :: ServerState -> Application
app serverState request respond = do
  case (requestMethod request, BS.unpack $ requestMethod request) of
    (method, "POST") | method == methodPost -> do
      body <- getRequestBodyChunk request
      case decode (LBS.fromStrict body) of
        Just gameRequest -> do
          let (response, newServerState) = processGameAction gameRequest serverState
          respond $ responseLBS status200 [("Content-Type", "application/json")] (encode response)
        Nothing -> do
          let errorResp = errorResponse "Invalid JSON"
          respond $ responseLBS status400 [("Content-Type", "application/json")] (encode errorResp)
    (method, "GET") | method == methodGet -> do
      let response = getStateResponse serverState
      respond $ responseLBS status200 [("Content-Type", "application/json")] (encode response)
    _ -> do
      let errorResp = errorResponse "Method not allowed"
      respond $ responseLBS status404 [("Content-Type", "application/json")] (encode errorResp)

-- CORS middleware
corsMiddleware :: Application -> Application
corsMiddleware app request respond = do
  app request $ \response -> do
    let headers =
          [ ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Methods", "GET, POST, OPTIONS"),
            ("Access-Control-Allow-Headers", "Content-Type")
          ]
    respond response

-- Start server
startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting Tic-Tac-Toe server on port " ++ show port
  run port (corsMiddleware (app initialServerState))
