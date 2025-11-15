{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Langchain.LLM.Core
import Langchain.PromptTemplate
import Langchain.Message (ChatMessage(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import qualified Data.Text.Encoding as TE
import Data.Aeson (object, (.=), encode, decode, Value(..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL

-- Kimi LLM implementation
data Kimi = Kimi
  { kimiApiKey    :: Text      -- ^ Your Kimi API key
  , kimiEndpoint  :: Text      -- ^ Base URL of the Kimi API
  , kimiModel     :: Text      -- ^ Model name, e.g. "kimi-k2-0905-preview"
  }

data KimiParams = KimiParams
  { kimiTemperature :: Maybe Double  -- ^ Sampling temperature (0.0 to 1.0)
  , kimiMaxTokens   :: Maybe Int     -- ^ Maximum tokens in output
  }

defaultKimiParams :: KimiParams
defaultKimiParams = KimiParams
  { kimiTemperature = Just 0.7
  , kimiMaxTokens = Just 1500
  }

-- Extract content from API response
extractContent :: BL.ByteString -> Maybe Text
extractContent body = do
  val <- decode body
  case val of
    Object obj -> do
      Object choices <- KM.lookup "choices" obj
      Object choice <- KM.lookup "0" (KM.singleton "0" choices)
      Object message <- KM.lookup "message" choice
      String content <- KM.lookup "content" message
      return content
    _ -> Nothing

instance LLM Kimi where
  type LLMParams Kimi = KimiParams

  generate Kimi{..} prompt mParams = do
    let params = fromMaybe defaultKimiParams mParams
        reqBody = object
          [ "model" .= kimiModel
          , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
          , "temperature" .= kimiTemperature params
          , "max_tokens" .= kimiMaxTokens params
          ]
        endpoint = T.unpack kimiEndpoint <> "/chat/completions"

    request <- parseRequest endpoint
    let req = setRequestMethod "POST"
            $ setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> kimiApiKey]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestBodyLBS (encode reqBody)
            $ request

    response <- httpLBS req
    let status = getResponseStatusCode response
        body   = getResponseBody response

    if status >= 200 && status < 300
      then case extractContent body of
        Just content -> return $ Right content
        Nothing -> return $ Right $ TE.decodeUtf8 $ BL.toStrict body  -- Fallback to full response
      else return $ Left $ "Kimi API error: HTTP " ++ show status ++ "; " ++ show (TE.decodeUtf8 $ BL.toStrict body)

  -- Chat method: accepts ChatMessage list
  chat kimi messages mParams = do
    -- Convert ChatMessage to Text and use generate
    let prompt = T.intercalate "\n" $ map chatMessageContent messages
    generate kimi prompt mParams

  -- Stream not implemented
  stream _ _ _ _ = return $ Left "Streaming not supported for Kimi"

-- Helper to extract content from ChatMessage
chatMessageContent :: ChatMessage -> Text
chatMessageContent (UserMessage txt) = txt
chatMessageContent (AssistantMessage txt) = txt
chatMessageContent (SystemMessage txt) = txt

main :: IO ()
main = do
  let kimiLLM = Kimi
        { kimiApiKey = "sk-mky1mf4lC9tVOCTLXqqe9KT0b3NCaNXsJ3PPO13vopiUaCSn"
        , kimiEndpoint = "https://api.moonshot.ai/v1"
        , kimiModel = "moonshot-v1-8k"
        }

  -- Example 1: Simple prompt using generate
  putStrLn "=== Example 1: Simple Generate ==="
  result1 <- generate kimiLLM "What is the capital of France?" Nothing
  case result1 of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn $ "Response: " ++ T.unpack response

  putStrLn "\n=== Example 2: Using PromptTemplate ==="
  let prompt = PromptTemplate "Translate the following English text to French: {text}"
      input = Map.fromList [("text", "Hello, how are you?")]

  case renderPrompt prompt input of
    Left e -> putStrLn $ "Error: " ++ e
    Right renderedPrompt -> do
      result2 <- generate kimiLLM renderedPrompt Nothing
      case result2 of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "Translation: " ++ T.unpack response

  putStrLn "\n=== Example 3: Using Chat with Messages ==="
  let messages =
        [ SystemMessage "You are a helpful assistant."
        , UserMessage "Tell me a joke about programming."
        ]

  result3 <- chat kimiLLM messages Nothing
  case result3 of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn $ "Joke: " ++ T.unpack response
