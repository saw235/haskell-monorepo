module Main where

import Server (logInfo, startServer)

main :: IO ()
main = do
  logInfo "Starting Tic-Tac-Toe Server..."
  logInfo "Server will be available at http://localhost:8081"
  startServer 8081
