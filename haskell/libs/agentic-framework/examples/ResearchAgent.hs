{-# LANGUAGE OverloadedStrings #-}

-- |
-- Research agent example demonstrating step-level capability application
-- This shows how capabilities can be applied to individual workflow steps
-- for fine-grained control over agent behavior.
module Main where

import AgenticFramework.Agent (Agent (..))
import Configuration.Dotenv (defaultConfig, loadFile)
import AgenticFramework.Types (LLMConfig (..), LLMProvider (..), Message (..), Tool (..), ToolError (..), ToolInput (..), ToolOutput (..), ToolSchema (..))
import AgenticFramework.Workflow
import AgenticFramework.Workflow.DSL
import AgenticFramework.Workflow.Types
import Data.Aeson (Value (..), object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

-- | Web search tool for research
searchTool :: Tool
searchTool =
  Tool
    { toolName = "web_search",
      toolDescription = "Searches the web for information",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput (String query)) -> do
        -- Simulate a web search result
        return $ Right $ ToolOutput $ String $
          "Search results for '" <> query <> "':\n" <>
          "1. Haskell is a purely functional programming language\n" <>
          "2. Haskell features strong static typing and lazy evaluation\n" <>
          "3. Monads are a core abstraction in Haskell",
      toolTimeout = Nothing,
      toolRetryable = True
    }

-- | Documentation search tool
docSearchTool :: Tool
docSearchTool =
  Tool
    { toolName = "doc_search",
      toolDescription = "Searches documentation for technical details",
      toolSchema = ToolSchema (object []) (object []),
      toolExecute = \(ToolInput (String query)) -> do
        -- Simulate documentation search
        return $ Right $ ToolOutput $ String $
          "Documentation for '" <> query <> "':\n" <>
          "- Type: Type class for monadic operations\n" <>
          "- Methods: >>= (bind), >> (then), return\n" <>
          "- Laws: Left identity, right identity, associativity",
      toolTimeout = Nothing,
      toolRetryable = True
    }

-- | Research workflow with step-level capabilities
-- Each step can have different capabilities applied
-- This workflow ACTUALLY CALLS THE LLM at each step
researchWorkflow :: Workflow Text
researchWorkflow = do
  query <- getUserPrompt

  -- Step 1: Initial analysis with reasoning capability
  -- Uses LLM to break down the query
  analysis <- withCapability "reasoning" $ do
    llmCall $ "Break down this research query into key components and suggest a research strategy: " <> query

  -- Step 2: Web search with conciseness capability
  -- Uses tool then LLM to summarize results
  webResults <- withCapability "conciseness" $ do
    toolResult <- useTool "web_search" (String query)
    case toolResult of
      String r -> llmCall $ "Summarize these search results concisely:\n" <> r
      _ -> return "No search results"

  -- Step 3: Documentation search with technical-writing capability
  -- Uses tool then LLM for technical explanation
  docResults <- withCapability "technical-writing" $ do
    toolResult <- useTool "doc_search" (String query)
    case toolResult of
      String r -> llmCall $ "Provide a technical explanation based on this documentation:\n" <> r
      _ -> return "No documentation found"

  -- Step 4: Final synthesis with creativity capability
  -- Uses LLM to combine all findings into a creative response
  synthesis <- withCapability "creativity" $ do
    llmCall $ "Create a comprehensive and engaging answer about '" <> query <> "' by synthesizing these findings:\n\n" <>
              "Analysis:\n" <> analysis <> "\n\n" <>
              "Web Results:\n" <> webResults <> "\n\n" <>
              "Documentation:\n" <> docResults

  return synthesis

-- | Simple workflow without step-level capabilities (for comparison)
-- Also calls the LLM but without step-specific capabilities
simpleWorkflow :: Workflow Text
simpleWorkflow = do
  query <- getUserPrompt
  -- Simple direct LLM call without capabilities
  llmCall $ "Answer this question briefly: " <> query

-- | Capability definitions
-- These modifiers prepend instructions that change how the LLM responds
reasoningCapability :: Capability
reasoningCapability =
  Capability
    { capName = "reasoning",
      capDescription = "Break down problems step-by-step, consider multiple angles",
      capModifier = \text ->
        "Think through this step-by-step. Consider multiple angles and break down the problem into components.\n\n" <> text,
      capParameters = Nothing
    }

concisenessCapability :: Capability
concisenessCapability =
  Capability
    { capName = "conciseness",
      capDescription = "Provide brief, to-the-point responses",
      capModifier = \text ->
        "Be concise and to the point. Keep your response brief - no more than 2-3 sentences.\n\n" <> text,
      capParameters = Nothing
    }

technicalWritingCapability :: Capability
technicalWritingCapability =
  Capability
    { capName = "technical-writing",
      capDescription = "Use precise technical language with examples",
      capModifier = \text ->
        "Use precise technical language. Include code examples where appropriate. Be accurate and detailed.\n\n" <> text,
      capParameters = Nothing
    }

creativityCapability :: Capability
creativityCapability =
  Capability
    { capName = "creativity",
      capDescription = "Think outside the box, provide creative explanations",
      capModifier = \text ->
        "Be creative and engaging. Use analogies and metaphors to explain concepts. Make it interesting and memorable.\n\n" <> text,
      capParameters = Nothing
    }

-- | Main entry point
main :: IO ()
main = do
  putStrLn "=== Research Agent Example ==="
  putStrLn "Demonstrating step-level capability application"
  putStrLn ""

  -- Load .env file and get API key from environment
  _ <- loadFile defaultConfig
  maybeApiKey <- lookupEnv "KIMI_API_KEY"
  apiKey <- case maybeApiKey of
    Nothing -> error "Error: KIMI_API_KEY not set in .env or environment"
    Just key -> return $ T.pack key

  -- Create agent context with multiple capabilities
  historyRef <- newIORef []
  let context =
        AgentContext
          { ctxSystemPrompt = "You are a research assistant",
            ctxUserPrompt = "What are monads in Haskell?",
            ctxTools = [searchTool, docSearchTool],
            ctxCapabilities =
              [ concisenessCapability,
                technicalWritingCapability
              ],
            ctxLLM =
              LLMConfig
                { llmProvider = Kimi,
                  llmModel = "moonshot-v1-8k",
                  llmApiKey = Just apiKey,
                  llmBaseUrl = Just "https://api.moonshot.ai/v1",
                  llmMaxTokens = 2000,
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

  -- Run the research workflow with step-level capabilities
  putStrLn "Running research workflow with step-level capabilities..."
  putStrLn "------------------------------------------------------------"
  result1 <- runWorkflow researchWorkflow context initialState
  TIO.putStrLn result1
  putStrLn ""

  -- Run simple workflow without step-level capabilities for comparison
  putStrLn "Running simple workflow (no step-level capabilities)..."
  putStrLn "------------------------------------------------------------"
  result2 <- runWorkflow simpleWorkflow context initialState
  TIO.putStrLn result2
  putStrLn ""

  putStrLn "=== Key Observations ==="
  putStrLn "1. Step-level capabilities allow fine-grained control"
  putStrLn "2. Each step can have different behavior modifiers"
  putStrLn "3. Capabilities are isolated between steps"
  putStrLn "4. Global capabilities apply to all steps (if set)"
