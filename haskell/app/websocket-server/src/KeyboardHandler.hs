{-# LANGUAGE OverloadedStrings #-}

module KeyboardHandler (handleKeyboardInput) where

import Data.Aeson (Object, Value (..), decode, encode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (lookup)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

createResponse :: Text -> Text -> L8.ByteString
createResponse msgType content = encode $ object ["type" .= msgType, "content" .= content]

handleKeyboardInput :: Text -> L8.ByteString
handleKeyboardInput keyData =
  case decode (L8.fromStrict $ T.encodeUtf8 keyData) of
    Just keyInfo ->
      let key = extractKey keyInfo
          direction = extractDirection keyInfo
          position = extractPosition keyInfo
          responseText = "Key: " <> key <> " (" <> direction <> ") at position " <> position
       in createResponse "keyboard_ack" responseText
    Nothing -> createResponse "keyboard_error" "Invalid keyboard data format"

extractKey :: Object -> Text
extractKey keyInfo =
  case Data.Aeson.KeyMap.lookup (fromText "key") keyInfo of
    Just (String k) -> k
    _ -> "unknown"

extractDirection :: Object -> Text
extractDirection keyInfo =
  case Data.Aeson.KeyMap.lookup (fromText "direction") keyInfo of
    Just (String d) -> d
    _ -> "unknown"

extractPosition :: Object -> Text
extractPosition keyInfo =
  case Data.Aeson.KeyMap.lookup (fromText "position") keyInfo of
    Just (Object pos) ->
      let x = extractCoordinate "x" pos
          y = extractCoordinate "y" pos
       in "(" <> T.pack x <> "," <> T.pack y <> ")"
    _ -> "(0,0)"

extractCoordinate :: Text -> Object -> String
extractCoordinate coord pos =
  case Data.Aeson.KeyMap.lookup (fromText coord) pos of
    Just (Number n) -> show (round n :: Integer)
    _ -> "0"
