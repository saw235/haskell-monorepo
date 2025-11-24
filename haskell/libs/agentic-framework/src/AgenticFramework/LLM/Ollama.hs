{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AgenticFramework.LLM.Ollama
-- Description : Ollama LLM implementation
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides an LLM instance for Ollama.
module AgenticFramework.LLM.Ollama
  ( OllamaLLM (..),
    OllamaParams (..),
    defaultOllamaParams,
    createOllamaLLM,
  )
where

import AgenticFramework.Types (LLMConfig (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import qualified Langchain.LLM.Core as LLM
import Network.HTTP.Conduit (responseTimeoutMicro)
import Network.HTTP.Simple

-- | Ollama LLM implementation
data OllamaLLM = OllamaLLM
  { ollamaModel :: Text,
    ollamaBaseUrl :: Text
  }
  deriving (Show)

-- | Parameters for Ollama requests
data OllamaParams = OllamaParams
  { ollamaTemperature :: Maybe Double,
    ollamaMaxTokens :: Maybe Int
  }
  deriving (Show)

-- | Default parameters
defaultOllamaParams :: OllamaParams
defaultOllamaParams =
  OllamaParams
    { ollamaTemperature = Just 0.7,
      ollamaMaxTokens = Just 4096
    }

-- | Create an Ollama LLM from our config
createOllamaLLM :: LLMConfig -> OllamaLLM
createOllamaLLM config =
  OllamaLLM
    { ollamaModel = llmModel config,
      ollamaBaseUrl = fromMaybe "http://localhost:11434" (llmBaseUrl config)
    }

-- | Extract content from Ollama API response
extractContent :: BL.ByteString -> Maybe Text
extractContent body = do
  val <- decode body
  case val of
    Object obj ->
      KM.lookup "response" obj >>= \case
        String content -> Just content
        _ -> Nothing
    _ -> Nothing

instance LLM.LLM OllamaLLM where
  type LLMParams OllamaLLM = OllamaParams

  generate OllamaLLM {..} prompt mParams = do
    let params = fromMaybe defaultOllamaParams mParams
        reqBody =
          object
            [ "model" .= ollamaModel,
              "prompt" .= prompt,
              "stream" .= False,
              "options"
                .= object
                  [ "temperature" .= ollamaTemperature params,
                    "num_predict" .= ollamaMaxTokens params
                  ]
            ]
        endpoint = T.unpack ollamaBaseUrl <> "/api/generate"

    request <- parseRequest endpoint
    let req =
          setRequestMethod "POST" $
            setRequestHeader "Content-Type" ["application/json"] $
              setRequestBodyLBS (encode reqBody) $
                setRequestResponseTimeout (responseTimeoutMicro (5 * 60 * 1000000)) $ -- 5 minute timeout
                  request

    response <- httpLBS req
    let status = getResponseStatusCode response
        body = getResponseBody response

    if status >= 200 && status < 300
      then case extractContent body of
        Just content -> return $ Right content
        Nothing -> return $ Left $ "Failed to parse Ollama response: " ++ show (TE.decodeUtf8 $ BL.toStrict body)
      else return $ Left $ "Ollama API error: HTTP " ++ show status

  -- Chat not implemented for now
  chat llm messages mParams = do
    let prompt = T.intercalate "\n" $ map LLM.content $ NE.toList messages
    LLM.generate llm prompt mParams

  -- Stream not implemented
  stream _ _ _ _ = return $ Left "Streaming not supported yet"
