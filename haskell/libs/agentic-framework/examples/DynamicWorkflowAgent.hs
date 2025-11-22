{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : DynamicWorkflowAgent
-- Description : Example agent using dynamically loaded capabilities in workflows
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This example demonstrates how to combine:
-- - User Story 2: Typed workflows with DSL
-- - User Story 3: Step-level capabilities
-- - User Story 5: Dynamic capability loading from JSON files
module Main where

import AgenticFramework.Agent (agentCapabilities)
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Tool (..))
import AgenticFramework.Workflow (runWorkflow_)
import AgenticFramework.Workflow.Builder
import AgenticFramework.Workflow.Capabilities (applyCapabilities, findCapability)
import AgenticFramework.Workflow.DSL (llmCall, useTool, branch, getUserPrompt)
import AgenticFramework.Workflow.Execution (nestedWorkflow, withLocalCapabilities, withNestedContext)
import AgenticFramework.Workflow.Loader
import AgenticFramework.Workflow.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify, gets)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Default path to example capabilities
exampleCapabilitiesDir :: FilePath
exampleCapabilitiesDir = "haskell/libs/agentic-framework/examples/capabilities"

--------------------------------------------------------------------------------
-- Workflows defined at top level - they read capabilities from context
--------------------------------------------------------------------------------

-- | Research workflow that reads capabilities from the agent context
--   This can be defined at top level because it looks up capabilities dynamically
researchWorkflow :: Workflow Text
researchWorkflow = do
  userPrompt <- getUserPrompt
  caps <- asks ctxCapabilities
  liftIO $ TIO.putStrLn $ "  User prompt: " <> userPrompt

  -- Find capabilities from context
  let reasoningCap = findCapability "reasoning" caps
      technicalCap = findCapability "technical" caps
      conciseCap = findCapability "concise" caps

  -- Step A: Apply reasoning capability for initial analysis
  liftIO $ putStrLn "  [Step A] Applying 'reasoning' capability..."
  let reasonedPrompt = capModifier reasoningCap userPrompt
  liftIO $ TIO.putStrLn $ "    Modified: " <> T.take 80 reasonedPrompt <> "..."

  -- Step B: Check if it's a technical question
  let isTechnical = T.isInfixOf "code" userPrompt || T.isInfixOf "function" userPrompt

  -- Step C: Apply appropriate capability based on question type
  result <- if isTechnical
    then do
      liftIO $ putStrLn "  [Step B] Technical question detected - applying 'technical' capability..."
      return $ capModifier technicalCap reasonedPrompt
    else do
      liftIO $ putStrLn "  [Step B] General question - applying 'concise' capability..."
      return $ capModifier conciseCap reasonedPrompt

  liftIO $ putStrLn "  [Step C] Workflow complete!"
  return result

-- | Code review workflow - also reads capabilities from context
codeReviewWorkflow :: Workflow Text
codeReviewWorkflow = do
  userPrompt <- getUserPrompt
  caps <- asks ctxCapabilities
  liftIO $ putStrLn "  [Code Review] Analyzing code..."

  let codeReviewCap = findCapability "code-review" caps
  let reviewPrompt = capModifier codeReviewCap userPrompt
  liftIO $ TIO.putStrLn $ "    Review prompt: " <> T.take 100 reviewPrompt <> "..."

  return reviewPrompt

-- | Nested workflow that composes multiple sub-workflows
--   Demonstrates: nestedWorkflow, withLocalCapabilities, withNestedContext
nestedResearchWorkflow :: Workflow Text
nestedResearchWorkflow = do
  userPrompt <- getUserPrompt
  caps <- asks ctxCapabilities
  liftIO $ TIO.putStrLn $ "  [Nested Main] Starting with: " <> T.take 50 userPrompt

  let reasoningCap = findCapability "reasoning" caps
      technicalCap = findCapability "technical" caps
      conciseCap = findCapability "concise" caps

  -- Sub-workflow 1: Analysis phase with reasoning capability
  analysisResult <- nestedWorkflow $ do
    liftIO $ putStrLn "    [Nested Sub-1] Running analysis sub-workflow..."
    prompt <- getUserPrompt
    let analyzed = capModifier reasoningCap prompt
    liftIO $ putStrLn "    [Nested Sub-1] Analysis complete"
    return analyzed

  -- Sub-workflow 2: Refinement phase with local capability scope
  refinedResult <- withLocalCapabilities [conciseCap] $ nestedWorkflow $ do
    liftIO $ putStrLn "    [Nested Sub-2] Running refinement sub-workflow..."
    let refined = capModifier conciseCap analysisResult
    liftIO $ putStrLn "    [Nested Sub-2] Refinement complete"
    return refined

  -- Sub-workflow 3: Final formatting with modified context
  let isTechnical = T.isInfixOf "code" userPrompt || T.isInfixOf "function" userPrompt
  finalResult <- withNestedContext (\c -> c { ctxSystemPrompt = "Format output professionally" }) $
    nestedWorkflow $ do
      liftIO $ putStrLn "    [Nested Sub-3] Running formatting sub-workflow..."
      let formatted = if isTechnical
            then capModifier technicalCap refinedResult
            else refinedResult
      liftIO $ putStrLn "    [Nested Sub-3] Formatting complete"
      return formatted

  liftIO $ putStrLn "  [Nested Main] All sub-workflows complete!"
  return finalResult


main :: IO ()
main = do
  putStrLn "=== Dynamic Workflow Agent Example ==="
  putStrLn ""

  -- Step 1: Load capabilities from JSON files
  putStrLn "Step 1: Loading capabilities from JSON files..."
  (errors, capabilities) <- loadCapabilitiesFromDirectory exampleCapabilitiesDir

  if not (null errors)
    then do
      putStrLn "Warning: Some capabilities failed to load:"
      mapM_ (putStrLn . ("  - " ++) . show) errors
    else putStrLn $ "Loaded " ++ show (length capabilities) ++ " capabilities"
  putStrLn ""

  -- Step 2: Workflows are defined at top level and read capabilities from context
  putStrLn "Step 2: Using top-level workflows that read capabilities from context..."
  putStrLn ""

  -- Create context for workflow execution
  historyRef <- newIORef []
  let ctx = AgentContext
        { ctxSystemPrompt = "You are a helpful AI assistant."
        , ctxUserPrompt = "How do I implement a binary search function?"
        , ctxTools = []
        , ctxCapabilities = capabilities  -- Capabilities passed via context
        , ctxLLM = LLMConfig
            { llmProvider = Custom "Kimi"
            , llmModel = "moonshot-v1-8k"
            , llmApiKey = Nothing
            , llmBaseUrl = Just "https://api.moonshot.cn/v1"
            , llmMaxTokens = 2000
            , llmTemperature = 0.7
            }
        , ctxHistory = historyRef
        }

  -- Execute simple research workflow (defined at top level)
  -- Note: Using runWorkflow_ which uses defaultWorkflowState internally
  putStrLn "--- Simple Research Workflow (Technical Question) ---"
  result1 <- runWorkflow_ researchWorkflow ctx
  putStrLn ""
  putStrLn "Final result preview:"
  TIO.putStrLn $ "  " <> T.take 150 result1 <> "..."
  putStrLn ""

  -- Execute NESTED workflow (also defined at top level)
  putStrLn "--- Nested Workflow Demo ---"
  result2 <- runWorkflow_ nestedResearchWorkflow ctx
  putStrLn ""
  putStrLn "Nested workflow result preview:"
  TIO.putStrLn $ "  " <> T.take 200 result2 <> "..."
  putStrLn ""

  -- Execute code review workflow with different prompt
  let ctx3 = ctx { ctxUserPrompt = "Review this code: def add(a, b): return a + b" }
  putStrLn "--- Code Review Workflow ---"
  result3 <- runWorkflow_ codeReviewWorkflow ctx3
  putStrLn ""
  putStrLn "Final result preview:"
  TIO.putStrLn $ "  " <> T.take 200 result3 <> "..."
  putStrLn ""

  -- Step 3: Build an agent with the workflow
  putStrLn "Step 3: Building agent with workflow and capabilities..."
  agent <- buildAgent $ do
    withSystemPrompt "You are a helpful AI research assistant."
    mapM_ withCapability capabilities
    withWorkflow researchWorkflow  -- Top-level workflow
    withLLM $ LLMConfig
      { llmProvider = Custom "Kimi"
      , llmModel = "moonshot-v1-8k"
      , llmApiKey = Nothing
      , llmBaseUrl = Just "https://api.moonshot.cn/v1"
      , llmMaxTokens = 2000
      , llmTemperature = 0.7
      }

  putStrLn $ "Agent built with " ++ show (length (agentCapabilities agent)) ++ " capabilities"
  putStrLn "Done!"
