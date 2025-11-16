{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Context
-- Description : Agent execution context and token management
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides the AgentContext type which tracks conversation history,
-- loaded skills, tool executions, and token usage metrics for an agent.
--
-- = Token Management
--
-- The context automatically tracks token usage and can trigger summarization
-- when the token limit is approached:
--
-- @
-- import AgenticFramework.Context
--
-- -- Get current token metrics
-- let metrics = getTokenMetrics context
-- putStrLn $ "Tokens used: " <> show (currentTokenCount metrics)
--          <> " / " <> show (modelTokenLimit metrics)
-- @
module AgenticFramework.Context
  ( -- * Context Types
    AgentContext (..),
    OrchestrationContext (..),

    -- * Context Creation and Management
    createContext,
    updateContext,
    addMessage,
    addToolExecution,

    -- * Token Metrics
    getTokenMetrics,
    updateTokenMetrics,
  )
where

import AgenticFramework.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Context Data Types
--------------------------------------------------------------------------------

-- | Execution state and environment for an agent.
data AgentContext = AgentContext
  { contextAgentId :: AgentId,
    contextConversation :: [Message],
    contextToolHistory :: [ToolExecution],
    contextLoadedSkills :: HashMap SkillLabel Text, -- Label -> Content
    contextTokenMetrics :: TokenMetrics,
    contextOrchestration :: Maybe OrchestrationContext,
    contextCallStack :: [AgentId], -- For circular detection
    contextMaxDepth :: Int -- Default 5 (FR-022)
  }
  deriving (Show, Eq, Generic)

-- | Orchestration-specific context for multi-agent workflows.
data OrchestrationContext = OrchestrationContext
  { orchWorkflowId :: Text,
    orchParentAgent :: Maybe AgentId,
    orchAgentRole :: Text
  }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Context Creation and Management (Stubs)
--------------------------------------------------------------------------------

-- | Create a new agent context.
createContext :: AgentId -> LLMConfig -> AgentContext
createContext aid llmCfg =
  AgentContext
    { contextAgentId = aid,
      contextConversation = [],
      contextToolHistory = [],
      contextLoadedSkills = HM.empty,
      contextTokenMetrics =
        TokenMetrics
          { currentTokenCount = 0,
            modelTokenLimit = llmMaxTokens llmCfg,
            percentageUsed = 0.0,
            summarizationTriggered = False,
            lastSummarization = Nothing
          },
      contextOrchestration = Nothing,
      contextCallStack = [],
      contextMaxDepth = 5
    }

-- | Update context with new data.
updateContext :: AgentContext -> AgentContext
updateContext ctx = ctx -- TODO: Implement

-- | Add a message to the conversation history.
addMessage :: Message -> AgentContext -> AgentContext
addMessage msg ctx =
  ctx
    { contextConversation = contextConversation ctx ++ [msg]
    }

-- | Add a tool execution record to the history.
addToolExecution :: ToolExecution -> AgentContext -> AgentContext
addToolExecution exec ctx =
  ctx
    { contextToolHistory = contextToolHistory ctx ++ [exec]
    }

--------------------------------------------------------------------------------
-- Token Metrics
--------------------------------------------------------------------------------

-- | Get current token metrics (read-only API per FR-002b).
getTokenMetrics :: AgentContext -> TokenMetrics
getTokenMetrics = contextTokenMetrics

-- | Update token metrics based on new text.
--   Internal function for context management.
updateTokenMetrics :: AgentContext -> Text -> IO AgentContext
updateTokenMetrics ctx newText = do
  -- TODO: Call tokenizer FFI to count tokens
  -- TODO: Update percentageUsed
  -- TODO: Check if summarization should be triggered
  return ctx
