module Main where

import qualified Network.WebSockets as WS
import WebSocketServer (application)
import Prelude

main :: IO ()
main = do
  Prelude.putStrLn "Starting Haskell WebSocket server on localhost:9160"
  WS.runServer "127.0.0.1" 9160 application
