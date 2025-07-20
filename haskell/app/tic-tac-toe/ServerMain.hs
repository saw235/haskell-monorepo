-- |
-- Module      : Main
-- Description : Tic-Tac-Toe HTTP Server - Main Entry Point
-- Copyright   : (c) 2024
-- License     : MIT
-- Maintainer  :
-- Stability   : experimental
-- Portability : POSIX
--
-- This module serves as the main entry point for the Tic-Tac-Toe HTTP server.
-- It initializes and starts the web server that provides a REST API for
-- the Tic-Tac-Toe game, allowing clients to interact with the game logic
-- over HTTP.
--
-- The server runs on port 8081 by default and provides endpoints for:
-- - Starting new games
-- - Making moves
-- - Getting current game state
--
-- This server is designed to work with the Electron frontend application
-- and provides a clean separation between the game logic and the web interface.
--
-- Architecture:
-- - Uses Warp web server for HTTP handling
-- - STM for thread-safe state management
-- - JSON API for client communication
-- - CORS support for web clients
--
-- Usage:
--     @
--     main :: IO ()
--     main = do
--       logInfo "Starting Tic-Tac-Toe Server..."
--       startServer 8081
--     @
--
-- API Endpoints:
-- - POST / - Start new game or make move
-- - GET / - Get current game state
--
-- Server will be available at http://localhost:8081
module Main where

import Server (logInfo, startServer)

-- |
-- Main entry point for the Tic-Tac-Toe HTTP server.
--
-- This function:
-- 1. Logs server startup information
-- 2. Starts the HTTP server on the specified port
-- 3. Handles incoming requests using the Server module
--
-- The server provides a REST API for the Tic-Tac-Toe game,
-- allowing clients to interact with the game logic over HTTP.
--
-- @return IO action that runs the HTTP server
main :: IO ()
main = do
  logInfo "Starting Tic-Tac-Toe Server..."
  logInfo "Server will be available at http://localhost:8081"
  startServer 8081
