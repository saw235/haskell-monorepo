module ServerMain where

import Server (startServer)

main :: IO ()
main = do
  putStrLn "Starting Tic-Tac-Toe Server..."
  putStrLn "Server will be available at http://localhost:3000"
  startServer 3000 