{-# LANGUAGE OverloadedStrings #-}

module Main where

import AgenticFramework.Agent (agentCapabilities, executeAgent, resultOutput)
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..))
import AgenticFramework.Workflow.Builder
import AgenticFramework.Workflow.Types (Capability (..))
import qualified Data.Text as T

-- | Define a simple capability
uppercaseCapability :: Capability
uppercaseCapability =
  Capability
    { capName = "uppercase",
      capDescription = "Convert prompt to uppercase",
      capModifier = T.toUpper,
      capParameters = Nothing
    }

main :: IO ()
main = do
  putStrLn "Building agent..."

  -- Create agent using the new builder DSL
  agent <- buildAgent $ do
    withSystemPrompt "You are a helpful assistant."
    withCapability uppercaseCapability
    -- Mock LLM config for example
    withLLM $
      LLMConfig
        { llmProvider = Custom "Mock",
          llmModel = "mock-model",
          llmApiKey = Nothing,
          llmBaseUrl = Nothing,
          llmMaxTokens = 1000,
          llmTemperature = 0.7
        }

  putStrLn "Agent built successfully."
  putStrLn $ "Agent has " ++ show (length (agentCapabilities agent)) ++ " capabilities."

  -- Execute agent (mock execution)
  -- In a real scenario, this would call the LLM
  -- For this example, we just verify the agent was built correctly
  putStrLn "Done."
