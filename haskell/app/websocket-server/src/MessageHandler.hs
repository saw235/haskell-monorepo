{-# LANGUAGE OverloadedStrings #-}

module MessageHandler
  ( createResponse,
    handleClientMessage,
    helpText,
  )
where

import Calculator (calculateExpression)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import KeyboardHandler (handleKeyboardInput)
import Types (ClientMessage (..))

createResponse :: Text -> Text -> L8.ByteString
createResponse msgType content = encode $ object ["type" .= msgType, "content" .= content]

handleClientMessage :: ClientMessage -> L8.ByteString
handleClientMessage (ClientMessage "ping" _) =
  createResponse "pong" "Pong from Haskell!"
handleClientMessage (ClientMessage "echo" content) =
  createResponse "echo_response" ("Echo: " <> content)
handleClientMessage (ClientMessage "calculate" expr) =
  let result = calculateExpression expr
   in createResponse "calculation_result" result
handleClientMessage (ClientMessage "help" _) =
  createResponse "help_response" helpText
handleClientMessage (ClientMessage "keyboard" keyData) =
  handleKeyboardInput keyData
handleClientMessage _ =
  createResponse "error" "Unknown message format. Try sending 'help' for available commands."

helpText :: Text
helpText = "Available commands:\n• ping - Test connectivity\n• echo [message] - Echo your message back\n• calculate [expression] - Math operations (e.g., '10 + 5' or '10+5')\n• keyboard - Real-time arrow key input (sent automatically)\n• help - Show this help message"
