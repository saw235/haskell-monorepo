{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Context.Summarization
-- Description : Context window summarization to prevent token limit overflow
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides context summarization functionality to keep conversations
-- within token limits. When context approaches 90% of the model's limit, older
-- messages are summarized while preserving recent interactions.
--
-- = Requirements
--
-- - FR-042: Log WARNING with token counts when summarization occurs
-- - FR-043: Preserve recent messages, summarize only older history
-- - FR-044: Fallback to hard truncation if summarization fails
-- - SC-012: Achieve ≥50% token reduction while preserving key information
module AgenticFramework.Context.Summarization
  ( -- * Summarization Functions
    summarizeContext,
    shouldTriggerSummarization,

    -- * Configuration
    SummarizationConfig (..),
    defaultSummarizationConfig,
  )
where

import AgenticFramework.Types
import AgenticFramework.Context (AgentContext (..), getTokenMetrics, updateTokenMetrics)
import AgenticFramework.LLM.Ollama (createOllamaLLM, defaultOllamaParams)
import AgenticFramework.LLM.Kimi (createKimiLLM, defaultKimiParams)
import qualified Langchain.LLM.Core as LLM
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for context summarization
data SummarizationConfig = SummarizationConfig
  { summarizationThreshold :: Double
    -- ^ Percentage of token limit that triggers summarization (default: 0.9 for 90%)
  , preserveRecentMessages :: Int
    -- ^ Number of recent messages to preserve without summarization (default: 5)
  , summarizationTimeout :: Int
    -- ^ Timeout for summarization in microseconds (default: 30 seconds)
  , fallbackToTruncation :: Bool
    -- ^ Whether to fallback to truncation if summarization fails (default: True)
  }
  deriving (Show, Eq)

-- | Default summarization configuration
--   Triggers at 90%, preserves last 5 messages, 30s timeout
defaultSummarizationConfig :: SummarizationConfig
defaultSummarizationConfig =
  SummarizationConfig
    { summarizationThreshold = 0.9,
      preserveRecentMessages = 5,
      summarizationTimeout = 30_000_000, -- 30 seconds
      fallbackToTruncation = True
    }

--------------------------------------------------------------------------------
-- Summarization Logic
--------------------------------------------------------------------------------

-- | Check if summarization should be triggered based on token usage
--   Returns True if percentage used >= threshold (default 90%)
shouldTriggerSummarization :: SummarizationConfig -> AgentContext -> Bool
shouldTriggerSummarization config ctx =
  let metrics = getTokenMetrics ctx
   in percentageUsed metrics >= summarizationThreshold config

-- | Summarize conversation context to reduce token usage
--
--   Strategy (FR-043):
--   1. Preserve the N most recent messages (default: 5)
--   2. Summarize older messages using the agent's LLM
--   3. Replace older messages with summary
--   4. Update token metrics
--
--   Error handling (FR-044):
--   - If summarization fails or times out, fallback to hard truncation
--   - Log ERROR with details of the failure
--
--   Returns updated context with reduced token count
summarizeContext :: LLMConfig -> SummarizationConfig -> AgentContext -> IO AgentContext
summarizeContext llmConfig config ctx = do
  let metrics = getTokenMetrics ctx
      conversation = contextConversation ctx
      recentCount = preserveRecentMessages config

  -- Split conversation into older (to summarize) and recent (to preserve)
  let (olderMessages, recentMessages) = splitAt (max 0 (length conversation - recentCount)) conversation

  if null olderMessages
    then do
      -- Nothing to summarize, return original context
      return ctx
    else do
      -- Attempt summarization with timeout
      summaryResult <- timeout (summarizationTimeout config) $
        try $ generateSummary llmConfig olderMessages

      case summaryResult of
        Nothing -> do
          -- Timeout occurred (FR-044)
          putStrLn $ "ERROR: Context summarization timed out after "
            <> show (summarizationTimeout config `div` 1_000_000) <> " seconds"
          putStrLn $ "Falling back to hard truncation. Original: "
            <> show (currentTokenCount metrics) <> " tokens"

          if fallbackToTruncation config
            then return $ hardTruncate config ctx
            else return ctx

        Just (Left (err :: SomeException)) -> do
          -- Summarization failed (FR-044)
          putStrLn $ "ERROR: Context summarization failed: " <> show err
          putStrLn $ "Falling back to hard truncation. Original: "
            <> show (currentTokenCount metrics) <> " tokens"

          if fallbackToTruncation config
            then return $ hardTruncate config ctx
            else return ctx

        Just (Right summary) -> do
          -- Summarization succeeded
          let originalTokens = currentTokenCount metrics

          -- Create summary message
          timestamp <- getCurrentTime
          let summaryMessage = SystemMessage
                { messageContent = "Previous conversation summary:\n\n" <> summary
                , messageTimestamp = timestamp
                }

          -- Build new conversation with summary + recent messages
          let newConversation = [summaryMessage] ++ recentMessages
          let newCtx = ctx { contextConversation = newConversation }

          -- Update token metrics (will be recalculated)
          newCtx' <- updateTokenMetrics newCtx summary

          let newMetrics = getTokenMetrics newCtx'
              newTokens = currentTokenCount newMetrics
              reduction = fromIntegral (originalTokens - newTokens) / fromIntegral originalTokens * 100

          -- Log summarization (FR-042)
          putStrLn $ "WARNING: Context summarization triggered"
          putStrLn $ "  Original token count: " <> show originalTokens
          putStrLn $ "  Summarized token count: " <> show newTokens
          putStrLn $ "  Token reduction: " <> show (round reduction :: Int) <> "%"
          putStrLn $ "  Messages summarized: " <> show (length olderMessages)
          putStrLn $ "  Messages preserved: " <> show (length recentMessages)

          -- Mark that summarization occurred
          let finalCtx = newCtx'
                { contextTokenMetrics = (contextTokenMetrics newCtx')
                    { summarizationTriggered = True
                    , lastSummarization = Just timestamp
                    }
                }

          return finalCtx

-- | Generate summary of older messages using LLM
generateSummary :: LLMConfig -> [Message] -> IO Text
generateSummary llmConfig messages = do
  -- Format messages for summarization
  let messageTexts = map formatMessage messages
      conversationText = T.intercalate "\n\n" messageTexts

      prompt = T.unlines
        [ "Please provide a concise summary of the following conversation."
        , "Focus on key facts, decisions, and context that would be important to remember."
        , "Be brief but comprehensive."
        , ""
        , "Conversation:"
        , conversationText
        ]

  -- Call LLM based on provider
  result <- case llmProvider llmConfig of
    Ollama -> do
      let llm = createOllamaLLM llmConfig
      LLM.generate llm prompt (Just defaultOllamaParams)

    Kimi -> do
      case createKimiLLM llmConfig of
        Nothing -> return $ Left "Kimi API key not provided"
        Just llm -> LLM.generate llm prompt (Just defaultKimiParams)

    _ -> return $ Left "LLM provider not supported for summarization"

  case result of
    Left err -> error $ "Failed to generate summary: " <> err
    Right summary -> return summary

-- | Format a message for summarization
formatMessage :: Message -> Text
formatMessage (UserMessage content timestamp) =
  "User: " <> content
formatMessage (AssistantMessage content timestamp) =
  "Assistant: " <> content
formatMessage (SystemMessage content timestamp) =
  "System: " <> content

-- | Hard truncation fallback (FR-044)
--   Keep only the most recent N messages
hardTruncate :: SummarizationConfig -> AgentContext -> AgentContext
hardTruncate config ctx =
  let conversation = contextConversation ctx
      recentCount = preserveRecentMessages config
      truncated = drop (max 0 (length conversation - recentCount)) conversation
  in ctx { contextConversation = truncated }
