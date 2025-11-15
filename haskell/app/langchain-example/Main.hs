{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Langchain.LLM.Core
import Langchain.PromptTemplate
import Langchain.Callback
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import qualified Data.Text.Encoding as TE
import Data.Aeson (object, (.=), encode)
import qualified Data.ByteString.Lazy as BL

-- Qwen LLM implementation
data Qwen = Qwen
  { qwApiKey    :: Text      -- ^ Your Qwen API key
  , qwEndpoint  :: Text      -- ^ Base URL of the Qwen API
  , qwModel     :: Text      -- ^ Model name, e.g. "qwen-turbo"
  }

data QwenParams = QwenParams
  { qwTemperature :: Maybe Double  -- ^ Sampling temperature
  , qwMaxTokens   :: Maybe Int     -- ^ Maximum tokens in output
  }

defaultQwenParams :: QwenParams
defaultQwenParams = QwenParams
  { qwTemperature = Just 0.7
  , qwMaxTokens = Just 1500
  }

instance LLM Qwen where
  type LLMParams Qwen = QwenParams

  generate Qwen{..} prompt mParams = do
    let params = fromMaybe defaultQwenParams mParams
        reqBody = object
          [ "model" .= qwModel
          , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
          , "temperature" .= qwTemperature params
          , "max_tokens" .= qwMaxTokens params
          ]

    request <- parseRequest (T.unpack qwEndpoint)
    let req = setRequestMethod "POST"
            $ setRequestHeader "Authorization" [TE.encodeUtf8 $ "Bearer " <> qwApiKey]
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestBodyLBS (encode reqBody)
            $ request

    response <- httpBS req
    let status = getResponseStatusCode response
        body   = TE.decodeUtf8 $ getResponseBody response
    return $ if status >= 200 && status < 300
      then Right body
      else Left $ "Qwen error: HTTP " ++ show status ++ "; " ++ T.unpack body

main :: IO ()
main = do
  let qwenLLM = Qwen
        { qwApiKey = "sk-0463077ca85348c98d700fc5158198a4"
        , qwEndpoint = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions"
        , qwModel = "qwen-turbo"
        }
      prompt = PromptTemplate "Translate the following English text to French: {text}"
      input = Map.fromList [("text", "Hello, how are you?")]

  case renderPrompt prompt input of
    Left e -> putStrLn $ "Error: " ++ e
    Right renderedPrompt -> do
      eRes <- generate qwenLLM renderedPrompt Nothing
      case eRes of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "Translation: " ++ (T.unpack response)
