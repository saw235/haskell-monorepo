{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

data ClientMessage = ClientMessage
  { msgType :: Text,
    content :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON ClientMessage
instance ToJSON ClientMessage

data ServerMessage = ServerMessage
  { serverMsgType :: Text,
    serverContent :: Text
  }
  deriving (Show, Eq)

-- | 'FromJSON' instance for 'ServerMessage'.
--
-- This instance defines how to parse a JSON object into a 'ServerMessage' value using the Aeson library.
--
-- The implementation uses applicative style with the operators '<$>' and '<*>':
--
--   * '<$>' (fmap): Applies the 'ServerMessage' constructor to the result of parsing the "type" field from the JSON object.
--   * '<*>' (ap): Applies the partially constructed 'ServerMessage' (with the "type" field) to the result of parsing the "content" field.
--
-- In effect, this code:
--
--   instance FromJSON ServerMessage where
--     parseJSON = withObject "ServerMessage" $ \o -> ServerMessage
--       <$> o .: "type"
--       <*> o .: "content"
--
-- Is equivalent to the following do-notation:
--
--   instance FromJSON ServerMessage where
--     parseJSON = withObject "ServerMessage" $ \o -> do
--       t <- o .: "type"
--       c <- o .: "content"
--       pure (ServerMessage t c)
--
-- This approach is concise and idiomatic for parsing multiple fields from a JSON object in Haskell.
instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage" $ \o -> ServerMessage
    <$> o .: "type"
    <*> o .: "content"

instance ToJSON ServerMessage where
  toJSON (ServerMessage msgType content) = object
    [ "type" .= msgType
    , "content" .= content
    ]