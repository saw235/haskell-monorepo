{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (finally)
import Control.Monad (forever, forM_)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=), pairs, Value(..), Object)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (lookup)
import Data.Semigroup ((<>))
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS
import Prelude hiding (putStrLn)
import qualified Prelude

data ClientMessage = ClientMessage
  { msgType :: Text,
    content :: Text
  }
  deriving (Generic, Show)

instance FromJSON ClientMessage

instance ToJSON ClientMessage

createResponse :: Text -> Text -> L8.ByteString
createResponse msgType content = encode $ object ["type" .= msgType, "content" .= content]


application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    Prelude.putStrLn "New client connected!"
    let welcomeMsg = createResponse "welcome" "Hello from Haskell server!"
    WS.sendTextData conn welcomeMsg
    
    forever $ do
      msg <- WS.receiveData conn
      Prelude.putStrLn $ "Received: " ++ L8.unpack msg
      
      case decode msg of
        Just (ClientMessage "ping" _) -> do
          Prelude.putStrLn "Received ping, sending pong"
          let pongMsg = createResponse "pong" "Pong from Haskell!"
          WS.sendTextData conn pongMsg
        Just (ClientMessage "echo" content) -> do
          Prelude.putStrLn $ "Echoing: " ++ T.unpack content
          let response = createResponse "echo_response" ("Echo: " <> content)
          WS.sendTextData conn response
        Just (ClientMessage "calculate" expr) -> do
          Prelude.putStrLn $ "Calculating: " ++ T.unpack expr
          let result = calculateExpression expr
          let response = createResponse "calculation_result" result
          WS.sendTextData conn response
        Just (ClientMessage "help" _) -> do
          Prelude.putStrLn "Sending help information"
          let helpText = "Available commands:\n• ping - Test connectivity\n• echo [message] - Echo your message back\n• calculate [expression] - Math operations (e.g., '10 + 5' or '10+5')\n• keyboard - Real-time arrow key input (sent automatically)\n• help - Show this help message"
          let response = createResponse "help_response" helpText
          WS.sendTextData conn response
        Just (ClientMessage "keyboard" keyData) -> do
          Prelude.putStrLn $ "Received keyboard input: " ++ T.unpack keyData
          let response = handleKeyboardInput keyData
          WS.sendTextData conn response
        _ -> do
          Prelude.putStrLn "Unknown message format"
          let errorMsg = createResponse "error" "Unknown message format. Try sending 'help' for available commands."
          WS.sendTextData conn errorMsg

handleKeyboardInput :: Text -> L8.ByteString
handleKeyboardInput keyData = 
  case decode (L8.fromStrict $ T.encodeUtf8 keyData) of
    Just keyInfo -> 
      let key = case Data.Aeson.KeyMap.lookup (fromText "key") keyInfo of
                  Just (String k) -> k
                  _ -> "unknown"
          direction = case Data.Aeson.KeyMap.lookup (fromText "direction") keyInfo of
                        Just (String d) -> d
                        _ -> "unknown"
          position = case Data.Aeson.KeyMap.lookup (fromText "position") keyInfo of
                       Just (Object pos) -> 
                         let x = case Data.Aeson.KeyMap.lookup (fromText "x") pos of
                                   Just (Number n) -> show (round n :: Integer)
                                   _ -> "0"
                             y = case Data.Aeson.KeyMap.lookup (fromText "y") pos of
                                   Just (Number n) -> show (round n :: Integer) 
                                   _ -> "0"
                         in "(" <> T.pack x <> "," <> T.pack y <> ")"
                       _ -> "(0,0)"
          responseText = "Key: " <> key <> " (" <> direction <> ") at position " <> position
      in createResponse "keyboard_ack" responseText
    Nothing -> createResponse "keyboard_error" "Invalid keyboard data format"

calculateExpression :: Text -> Text
calculateExpression expr = 
  -- Try with spaces first, then without spaces
  case parseWithSpaces expr of
    Just result -> result
    Nothing -> parseWithoutSpaces expr
  where
    parseWithSpaces expr = 
      case T.words expr of
        [a, "+", b] -> case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
          (Just x, Just y) -> Just $ T.pack $ show (x + y :: Double)
          _ -> Just "Invalid numbers"
        [a, "-", b] -> case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
          (Just x, Just y) -> Just $ T.pack $ show (x - y :: Double)
          _ -> Just "Invalid numbers"
        [a, "*", b] -> case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
          (Just x, Just y) -> Just $ T.pack $ show (x * y :: Double)
          _ -> Just "Invalid numbers"
        [a, "/", b] -> case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
          (Just x, Just y) -> if y /= 0 
            then Just $ T.pack $ show (x / y :: Double)
            else Just "Division by zero"
          _ -> Just "Invalid numbers"
        _ -> Nothing
    
    parseWithoutSpaces expr =
      let exprStr = T.unpack expr
      in case parseSimpleExpr exprStr of
           Just result -> T.pack $ show result
           Nothing -> "Invalid expression format. Use: number operator number (e.g., '10 + 5' or '10+5')"
    
    parseSimpleExpr :: String -> Maybe Double
    parseSimpleExpr s = 
      case break (`elem` ("+-*/" :: String)) s of
        (num1Str, op:num2Str) -> do
          num1 <- readMaybe num1Str
          num2 <- readMaybe num2Str
          case op of
            '+' -> Just (num1 + num2)
            '-' -> Just (num1 - num2)
            '*' -> Just (num1 * num2)
            '/' -> if num2 /= 0 then Just (num1 / num2) else Nothing
            _ -> Nothing
        _ -> Nothing
    
    readMaybe :: String -> Maybe Double
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

main :: IO ()
main = do
  Prelude.putStrLn "Starting Haskell WebSocket server on localhost:9160"
  WS.runServer "127.0.0.1" 9160 application