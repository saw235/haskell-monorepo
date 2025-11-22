{-# LANGUAGE OverloadedStrings #-}

-- |
-- Simple workflow example demonstrating basic workflow composition
module Main where

import AgenticFramework.Agent (Agent (..))
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Message (..), Tool (..), ToolError (..), ToolInput (..), ToolOutput (..), ToolSchema (..))
import AgenticFramework.Workflow
import AgenticFramework.Workflow.DSL
import AgenticFramework.Workflow.Types
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

-- | A simple calculator tool
calculatorTool :: Tool
calculatorTool =
  Tool
    { toolName = "calculator",
      toolDescription = "Performs basic arithmetic operations",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput val) -> do
        -- Simple echo for demonstration
        return $ Right $ ToolOutput val,
      toolTimeout = Nothing,
      toolRetryable = False
    }

-- | A simple search tool
searchTool :: Tool
searchTool =
  Tool
    { toolName = "search",
      toolDescription = "Searches for information",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput (String query)) -> do
        return $ Right $ ToolOutput $ String $ "Search results for: " <> query,
      toolTimeout = Nothing,
      toolRetryable = False
    }

-- | Simple analysis workflow
analysisWorkflow :: Workflow Text
analysisWorkflow = do
  -- Get the user's input
  userQuery <- getUserPrompt

  -- Check if it's a calculation or search query
  let isCalculation = T.isInfixOf "calculate" userQuery || T.isInfixOf "math" userQuery

  -- Branch based on query type
  result <-
    branch
      (return isCalculation)
      ( do
          -- Calculation path
          _ <- useTool "calculator" (String userQuery)
          return "Performed calculation"
      )
      ( do
          -- Search path
          searchResult <- useTool "search" (String userQuery)
          case searchResult of
            String res -> return res
            _ -> return "Search completed"
      )

  return $ "Workflow result: " <> result

-- | Parallel workflow example
parallelWorkflow :: Workflow Text
parallelWorkflow = do
  userQuery <- getUserPrompt

  -- Run multiple searches in parallel
  results <-
    parallel
      [ useTool "search" (String $ userQuery <> " wikipedia"),
        useTool "search" (String $ userQuery <> " documentation"),
        useTool "search" (String $ userQuery <> " tutorial")
      ]

  -- Combine results
  let combined = T.intercalate "\n" [r | String r <- results]
  return $ "Combined results:\n" <> combined

-- | Main entry point
main :: IO ()
main = do
  putStrLn "=== Simple Workflow Example ==="
  putStrLn ""

  -- Load .env file and get API key from environment
  _ <- loadFile defaultConfig
  maybeApiKey <- lookupEnv "KIMI_API_KEY"
  apiKey <- case maybeApiKey of
    Nothing -> error "Error: KIMI_API_KEY not set in .env or environment"
    Just key -> return $ T.pack key

  -- Create agent context
  historyRef <- newIORef []
  let context =
        AgentContext
          { ctxSystemPrompt = "You are a helpful assistant",
            ctxUserPrompt = "How do I calculate 2 + 2?",
            ctxTools = [calculatorTool, searchTool],
            ctxCapabilities = [],
            ctxLLM =
              LLMConfig
                { llmProvider = Kimi,
                  llmModel = "moonshot-v1-8k",
                  llmApiKey = Just apiKey,
                  llmBaseUrl = Just "https://api.moonshot.ai/v1",
                  llmMaxTokens = 1000,
                  llmTemperature = 0.7
                },
            ctxHistory = historyRef
          }

  let initialState =
        WorkflowState
          { stCurrentPhase = Init,
            stVariables = [],
            stStepCount = 0,
            stActiveCapabilities = []
          }

  -- Run the analysis workflow
  putStrLn "Running analysis workflow..."
  result1 <- runWorkflow analysisWorkflow context initialState
  TIO.putStrLn $ "Result: " <> result1
  putStrLn ""

  -- Run the parallel workflow
  putStrLn "Running parallel workflow..."
  let context2 = context {ctxUserPrompt = "What is Haskell?"}
  result2 <- runWorkflow parallelWorkflow context2 initialState
  TIO.putStrLn $ "Result: " <> result2
  putStrLn ""

  putStrLn "=== Example Complete ==="
