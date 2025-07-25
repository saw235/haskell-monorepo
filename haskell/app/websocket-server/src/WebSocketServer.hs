{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer (application) where

import Control.Monad (forever)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as L8
import MessageHandler (createResponse, handleClientMessage)
import qualified Network.WebSockets as WS
import Types (ClientMessage (..))
import Prelude hiding (putStrLn)
import qualified Prelude

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    logMessage "New client connected!"
    sendWelcomeMessage conn
    handleMessages conn

sendWelcomeMessage :: WS.Connection -> IO ()
sendWelcomeMessage conn = do
  let welcomeMsg = createResponse "welcome" "Hello from Haskell server!"
  WS.sendTextData conn welcomeMsg

handleMessages :: WS.Connection -> IO ()
handleMessages conn = forever $ do
  msg <- WS.receiveData conn
  logMessage $ "Received: " ++ L8.unpack msg

  case decode msg of
    Just clientMsg -> do
      logClientMessage clientMsg
      let response = handleClientMessage clientMsg
      WS.sendTextData conn response
    Nothing -> do
      logMessage "Unknown message format"
      let errorMsg = createResponse "error" "Unknown message format. Try sending 'help' for available commands."
      WS.sendTextData conn errorMsg

logMessage :: String -> IO ()
logMessage = Prelude.putStrLn

logClientMessage :: ClientMessage -> IO ()
logClientMessage (ClientMessage "ping" _) =
  logMessage "Received ping, sending pong"
logClientMessage (ClientMessage "echo" content) =
  logMessage $ "Echoing: " ++ show content
logClientMessage (ClientMessage "calculate" expr) =
  logMessage $ "Calculating: " ++ show expr
logClientMessage (ClientMessage "help" _) =
  logMessage "Sending help information"
logClientMessage (ClientMessage "keyboard" keyData) =
  logMessage $ "Received keyboard input: " ++ show keyData
logClientMessage _ =
  logMessage "Processing unknown message type"
