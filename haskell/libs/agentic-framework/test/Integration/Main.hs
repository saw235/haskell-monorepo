module Main (main) where

import AgenticFramework.Types (LLMConfig (..), LLMProvider (..))
import qualified Data.Text as T
import qualified Integration.ContextManagementSpec
import qualified Integration.ExecutionModeSpec
import qualified Integration.SimpleAgentSpec
import System.Environment (lookupEnv)
import Test.Hspec

-- | Create LLM config based on environment variables
-- Environment variables:
--   TEST_LLM_PROVIDER: "kimi" (default) or "ollama"
--   KIMI_API_KEY: API key (defaults to hardcoded key)
--   TEST_LLM_MODEL: Model name (default: "moonshot-v1-8k" for Kimi, "qwen3" for Ollama)
createTestLLMConfig :: IO LLMConfig
createTestLLMConfig = do
  providerStr <- lookupEnv "TEST_LLM_PROVIDER"
  apiKey <- lookupEnv "KIMI_API_KEY"
  modelName <- lookupEnv "TEST_LLM_MODEL"

  let provider = case providerStr of
        Just "ollama" -> Ollama
        _ -> Kimi -- Default to Kimi
  let (defaultModel, baseUrl, key) = case provider of
        Kimi -> ("moonshot-v1-8k", Just (T.pack "https://api.moonshot.ai/v1"), T.pack <$> apiKey)
        Ollama -> ("qwen3", Just (T.pack "http://localhost:11434"), Nothing)
        _ -> ("moonshot-v1-8k", Just (T.pack "https://api.moonshot.ai/v1"), T.pack <$> apiKey)

  let model = T.pack $ maybe defaultModel id modelName

  return $
    LLMConfig
      { llmProvider = provider,
        llmModel = model,
        llmApiKey = key,
        llmBaseUrl = baseUrl,
        llmMaxTokens = 4096,
        llmTemperature = 0.7
      }

main :: IO ()
main = do
  llmConfig <- createTestLLMConfig
  hspec $ do
    describe "Simple Agent" $ Integration.SimpleAgentSpec.spec llmConfig
    describe "Context Management" $ Integration.ContextManagementSpec.spec llmConfig
    describe "Execution Mode" $ Integration.ExecutionModeSpec.spec llmConfig
