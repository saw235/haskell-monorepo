{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ReasoningAgent
-- Description : Example agent demonstrating formal logical reasoning
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This example shows how to use the Reasoning module for formal logic:
-- - Modus Ponens: If P then Q. P. Therefore Q.
-- - Modus Tollens: If P then Q. Not Q. Therefore not P.
--
-- Requires KIMI_API_KEY environment variable to be set.
module Main where

import AgenticFramework.Types (LLMConfig (..), LLMProvider (..))
import Configuration.Dotenv (defaultConfig, loadFile)
import AgenticFramework.Workflow (runWorkflow_)
import AgenticFramework.Workflow.Reasoning
import AgenticFramework.Workflow.Types (AgentContext (..))
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "=== Formal Reasoning Agent ==="
  putStrLn ""
  putStrLn "This agent uses classical logic inference rules:"
  putStrLn "  • Modus Ponens: P → Q, P ⊢ Q"
  putStrLn "  • Modus Tollens: P → Q, ¬Q ⊢ ¬P"
  putStrLn ""

  -- Load .env file and get API key from environment
  _ <- loadFile defaultConfig
  maybeApiKey <- lookupEnv "KIMI_API_KEY"
  apiKey <- case maybeApiKey of
    Nothing -> error "Error: KIMI_API_KEY not set in .env or environment"
    Just key -> return $ T.pack key

  -- Create LLM config (using Kimi provider)
  historyRef <- newIORef []
  let llmConfig = LLMConfig
        { llmProvider = Kimi
        , llmModel = "moonshot-v1-8k"
        , llmApiKey = Just apiKey
        , llmBaseUrl = Just "https://api.moonshot.ai/v1"
        , llmMaxTokens = 2000
        , llmTemperature = 0.3  -- Lower temperature for more deterministic reasoning
        }

  -- Example 1: Modus Ponens
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn "Example 1: MODUS PONENS"
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn ""

  let modusPonensObservation = T.unlines
        [ "Observation 1: If it is raining, then the ground is wet."
        , "Observation 2: It is raining."
        ]

  TIO.putStrLn $ "Input observations:\n" <> modusPonensObservation
  putStrLn "Applying modus ponens..."
  putStrLn ""

  let ctx1 = AgentContext
        { ctxSystemPrompt = "You are a formal logic reasoner."
        , ctxUserPrompt = modusPonensObservation
        , ctxTools = []
        , ctxCapabilities = []
        , ctxLLM = llmConfig
        , ctxHistory = historyRef
        }

  result1 <- runWorkflow_ modusPonensWorkflow ctx1
  putStrLn "LLM Response:"
  putStrLn "─────────────"
  TIO.putStrLn result1
  putStrLn ""

  -- Example 2: Modus Tollens
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn "Example 2: MODUS TOLLENS"
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn ""

  let modusTollensObservation = T.unlines
        [ "Observation 1: If the car has fuel, then the car can start."
        , "Observation 2: The car cannot start."
        ]

  TIO.putStrLn $ "Input observations:\n" <> modusTollensObservation
  putStrLn "Applying modus tollens..."
  putStrLn ""

  let ctx2 = ctx1 { ctxUserPrompt = modusTollensObservation }

  result2 <- runWorkflow_ modusTollensWorkflow ctx2
  putStrLn "LLM Response:"
  putStrLn "─────────────"
  TIO.putStrLn result2
  putStrLn ""

  -- Example 3: General reasoning (auto-detect inference rule)
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn "Example 3: AUTO-DETECT INFERENCE RULE"
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn ""

  let generalObservation = T.unlines
        [ "Facts:"
        , "- All mammals are warm-blooded."
        , "- Whales are mammals."
        , ""
        , "Question: What can we conclude about whales?"
        ]

  TIO.putStrLn $ "Input observations:\n" <> generalObservation
  putStrLn "Applying formal reasoning..."
  putStrLn ""

  let ctx3 = ctx1 { ctxUserPrompt = generalObservation }

  result3 <- runWorkflow_ reasoningWorkflow ctx3
  putStrLn "LLM Response:"
  putStrLn "─────────────"
  TIO.putStrLn result3
  putStrLn ""

  -- Example 4: Invalid inference detection
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn "Example 4: INVALID INFERENCE (Affirming the Consequent)"
  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn ""

  let invalidObservation = T.unlines
        [ "Observation 1: If it is raining, then the ground is wet."
        , "Observation 2: The ground is wet."
        , ""
        , "Question: Can we conclude that it is raining?"
        ]

  TIO.putStrLn $ "Input observations:\n" <> invalidObservation
  putStrLn "Checking if valid inference is possible..."
  putStrLn "(Note: Affirming the consequent is a FALLACY)"
  putStrLn ""

  let ctx4 = ctx1 { ctxUserPrompt = invalidObservation }

  result4 <- runWorkflow_ reasoningWorkflow ctx4
  putStrLn "LLM Response:"
  putStrLn "─────────────"
  TIO.putStrLn result4
  putStrLn ""

  putStrLn "══════════════════════════════════════════════════════════════"
  putStrLn "Done!"
