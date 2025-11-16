module Main (main) where

import AgenticFramework.Types (LLMConfig (..), LLMProvider (..))
import qualified Data.Text as T
import qualified Integration.ContextManagementSpec
import qualified Integration.SimpleAgentSpec
import System.Environment (lookupEnv)
import Test.Hspec

-- | Create LLM config based on environment variables
-- Environment variables:
--   TEST_LLM_PROVIDER: "ollama" (default) or "kimi"
--   KIMI_API_KEY: Required when using Kimi provider
--   TEST_LLM_MODEL: Model name (default: "qwen3" for Ollama, "moonshot-v1-8k" for Kimi)
createTestLLMConfig :: IO LLMConfig
createTestLLMConfig = do
  providerStr <- lookupEnv "TEST_LLM_PROVIDER"
  apiKey <- lookupEnv "KIMI_API_KEY"
  modelName <- lookupEnv "TEST_LLM_MODEL"

  let provider = case providerStr of
        Just "kimi" -> Kimi
        _ -> Ollama  -- Default to Ollama

  let (defaultModel, baseUrl, key) = case provider of
        Kimi -> ("moonshot-v1-8k", Just (T.pack "https://api.moonshot.ai/v1"), fmap T.pack apiKey)
        Ollama -> ("qwen3", Just (T.pack "http://localhost:11434"), Nothing)
        _ -> ("qwen3", Nothing, Nothing)

  let model = T.pack $ maybe defaultModel id modelName

  return $ LLMConfig
    { llmProvider = provider
    , llmModel = model
    , llmApiKey = key
    , llmBaseUrl = baseUrl
    , llmMaxTokens = 4096
    , llmTemperature = 0.7
    }

main :: IO ()
main = do
  llmConfig <- createTestLLMConfig
  hspec $ do
    describe "Simple Agent" $ Integration.SimpleAgentSpec.spec llmConfig
    describe "Context Management" $ Integration.ContextManagementSpec.spec llmConfig
