{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Langchain.LLM.Core
import Langchain.PromptTemplate
import Network.HTTP.Simple
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv)

-- Kimi LLM implementation
data Kimi = Kimi
  { -- | Your Kimi API key
    kimiApiKey :: Text,
    -- | Base URL of the Kimi API
    kimiEndpoint :: Text,
    -- | Model name, e.g. "kimi-k2-0905-preview"
    kimiModel :: Text
  }

data KimiParams = KimiParams
  { -- | Sampling temperature (0.0 to 1.0)
    kimiTemperature :: Maybe Double,
    -- | Maximum tokens in output
    kimiMaxTokens :: Maybe Int
  }

defaultKimiParams :: KimiParams
defaultKimiParams =
  KimiParams
    { kimiTemperature = Just 0.7,
      kimiMaxTokens = Just 1500
    }

-- Extract content from API response
extractContent :: BL.ByteString -> Maybe Text
extractContent body = do
  val <- decode body
  case val of
    Object obj -> do
      Array choices <- KM.lookup "choices" obj
      case choices Vector.!? 0 of
        Just (Object choice) -> do
          Object message <- KM.lookup "message" choice
          String content <- KM.lookup "content" message
          return content
        _ -> Nothing
    _ -> Nothing

instance LLM Kimi where
  type LLMParams Kimi = KimiParams

  generate Kimi {..} prompt mParams = do
    let params = fromMaybe defaultKimiParams mParams
        reqBody =
          object
            [ "model" .= kimiModel,
              "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]],
              "temperature" .= kimiTemperature params,
              "max_tokens" .= kimiMaxTokens params
            ]
        endpoint = T.unpack kimiEndpoint <> "/chat/completions"

    request <- parseRequest endpoint
    let req =
          setRequestMethod "POST" $
            setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> kimiApiKey] $
              setRequestHeader "Content-Type" ["application/json"] $
                setRequestBodyLBS (encode reqBody) $
                  request

    response <- httpLBS req
    let status = getResponseStatusCode response
        body = getResponseBody response

    if status >= 200 && status < 300
      then case extractContent body of
        Just content -> return $ Right content
        Nothing -> return $ Right $ TE.decodeUtf8 $ BL.toStrict body -- Fallback to full response
      else return $ Left $ "Kimi API error: HTTP " ++ show status ++ "; " ++ show (TE.decodeUtf8 $ BL.toStrict body)

  -- Chat method: accepts ChatMessage (NonEmpty Message)
  chat kimi messages mParams = do
    -- Convert ChatMessage to Text and use generate
    let prompt = T.intercalate "\n" $ map messageContent $ Data.List.NonEmpty.toList messages
    generate kimi prompt mParams

  -- Stream not implemented
  stream _ _ _ _ = return $ Left "Streaming not supported for Kimi"

-- Helper to extract content from Message
messageContent :: Message -> Text
messageContent msg = content msg

-- Simple .env file loader
-- Parses lines like "KEY=value" and sets environment variables
loadEnvFile :: FilePath -> IO ()
loadEnvFile path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      let textLines = T.lines $ T.pack content
          trimmedLines = map T.strip textLines
          nonEmptyLines = filter (not . T.null) trimmedLines
          validLines = filter (not . T.isPrefixOf "#") nonEmptyLines -- Skip comments
      forM_ validLines $ \line ->
        case T.breakOn "=" line of
          (key, value) | not (T.null value) -> do
            let val = T.tail value -- Remove the '=' character
                key' = T.unpack $ T.strip key
                val' = T.unpack $ T.strip val
            unless (null key' || null val') $ do
              existing <- lookupEnv key'
              case existing of
                Nothing -> setEnv key' val' -- Only set if not already set
                Just _ -> return () -- Don't override existing env vars
          _ -> return () -- Skip invalid lines
    else return () -- Silently ignore if file doesn't exist

main :: IO ()
main = do
  -- Load .env file if it exists (tries current directory first)
  loadEnvFile ".env" `catch` \(_ :: SomeException) -> return ()

  -- Get configuration from environment variables with defaults
  apiKeyStr <- fromMaybe "your-api-key-here" <$> lookupEnv "KIMI_API_KEY"
  endpointStr <- fromMaybe "https://api.moonshot.ai/v1" <$> lookupEnv "KIMI_ENDPOINT"
  modelStr <- fromMaybe "moonshot-v1-8k" <$> lookupEnv "KIMI_MODEL"

  let kimiLLM =
        Kimi
          { kimiApiKey = T.pack apiKeyStr,
            kimiEndpoint = T.pack endpointStr,
            kimiModel = T.pack modelStr
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
  let systemMsg =
        Message
          { role = System,
            content = "You are a helpful assistant.",
            messageData = defaultMessageData
          }
      userMsg =
        Message
          { role = User,
            content = "Tell me a joke about programming.",
            messageData = defaultMessageData
          }
      messages = systemMsg :| [userMsg]

  result3 <- chat kimiLLM messages Nothing
  case result3 of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn $ "Joke: " ++ T.unpack response
