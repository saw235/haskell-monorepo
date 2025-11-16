{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AgenticFramework.LLM.Kimi
-- Description : Kimi LLM implementation
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides an LLM instance for Kimi (Moonshot AI).
module AgenticFramework.LLM.Kimi
  ( KimiLLM (..),
    KimiParams (..),
    defaultKimiParams,
    createKimiLLM,
  )
where

import AgenticFramework.Types (LLMConfig (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import qualified Langchain.LLM.Core as LLM
import Network.HTTP.Simple

-- | Kimi LLM implementation
data KimiLLM = KimiLLM
  { kimiModel :: Text,
    kimiApiKey :: Text,
    kimiBaseUrl :: Text
  }
  deriving (Show)

-- | Parameters for Kimi requests
data KimiParams = KimiParams
  { kimiTemperature :: Maybe Double,
    kimiMaxTokens :: Maybe Int
  }
  deriving (Show)

-- | Default parameters
defaultKimiParams :: KimiParams
defaultKimiParams =
  KimiParams
    { kimiTemperature = Just 0.7,
      kimiMaxTokens = Just 4096
    }

-- | Create a Kimi LLM from our config
createKimiLLM :: LLMConfig -> Maybe KimiLLM
createKimiLLM config = do
  apiKey <- llmApiKey config
  return
    KimiLLM
      { kimiModel = llmModel config,
        kimiApiKey = apiKey,
        kimiBaseUrl = fromMaybe "https://api.moonshot.ai/v1" (llmBaseUrl config)
      }

-- | Extract content from Kimi API response
extractContent :: BL.ByteString -> Maybe Text
extractContent body = do
  val <- decode body
  case val of
    Object obj -> do
      choices <- KM.lookup "choices" obj
      case choices of
        Array arr -> do
          if Vector.null arr
            then Nothing
            else case Vector.head arr of
              Object choice -> do
                message <- KM.lookup "message" choice
                case message of
                  Object msg ->
                    KM.lookup "content" msg >>= \case
                      String content -> Just content
                      _ -> Nothing
                  _ -> Nothing
              _ -> Nothing
        _ -> Nothing
    _ -> Nothing

instance LLM.LLM KimiLLM where
  type LLMParams KimiLLM = KimiParams

  generate KimiLLM {..} prompt mParams = do
    let params = fromMaybe defaultKimiParams mParams
        reqBody =
          object
            [ "model" .= kimiModel,
              "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]],
              "temperature" .= kimiTemperature params,
              "max_tokens" .= kimiMaxTokens params
            ]
        endpoint = T.unpack kimiBaseUrl <> "/chat/completions"

    request <- parseRequest endpoint
    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> kimiApiKey] $
                setRequestBodyLBS (encode reqBody) $
                  request

    response <- httpLBS req
    let status = getResponseStatusCode response
        body = getResponseBody response

    if status >= 200 && status < 300
      then case extractContent body of
        Just content -> return $ Right content
        Nothing -> return $ Left $ "Failed to parse Kimi response: " ++ show (TE.decodeUtf8 $ BL.toStrict body)
      else return $ Left $ "Kimi API error: HTTP " ++ show status ++ "; " ++ show (TE.decodeUtf8 $ BL.toStrict body)

  -- Chat method: accepts NonEmpty Message
  chat llm messages mParams = do
    let prompt = T.intercalate "\n" $ map LLM.content $ NE.toList messages
    LLM.generate llm prompt mParams

  -- Stream not implemented
  stream _ _ _ _ = return $ Left "Streaming not supported yet"
