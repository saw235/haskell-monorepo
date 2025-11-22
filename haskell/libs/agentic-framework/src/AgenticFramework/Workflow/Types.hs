{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : AgenticFramework.Workflow.Types
-- Description : Core types for the workflow system
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.Types
  ( -- * Core Workflow Types
    Workflow (..),
    WorkflowPhase (..),
    WorkflowState (..),
    AgentContext (..),

    -- * Capability Types
    Capability (..),
    CapabilityDef (..),

    -- * Error Types
    WorkflowError (..),
  )
where

import AgenticFramework.Types (LLMConfig, Message, Tool (..), ToolError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State (MonadState, StateT)
import Data.Aeson (Value)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Phases of a workflow lifecycle
data WorkflowPhase
  = Init
  | Configured
  | Ready
  | Executing
  | Complete
  deriving (Show, Eq, Generic)

-- | Core Workflow Monad
--   Uses ReaderT for context and StateT for mutable state over IO
newtype Workflow a = Workflow
  { unWorkflow :: ReaderT AgentContext (StateT WorkflowState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AgentContext,
      MonadState WorkflowState,
      Generic
    )


-- | Runtime context available to workflows
data AgentContext = AgentContext
  { ctxSystemPrompt :: Text,
    ctxUserPrompt :: Text,
    ctxTools :: [Tool],
    ctxCapabilities :: [Capability],
    ctxLLM :: LLMConfig,
    ctxHistory :: IORef [Message]
  }
  deriving (Generic)

instance Show AgentContext where
  show ctx = "AgentContext { "
    ++ "ctxSystemPrompt = " ++ show (ctxSystemPrompt ctx)
    ++ ", ctxUserPrompt = " ++ show (ctxUserPrompt ctx)
    ++ ", ctxTools = " ++ show (map toolName (ctxTools ctx))
    ++ ", ctxCapabilities = " ++ show (map capName (ctxCapabilities ctx))
    ++ ", ctxLLM = " ++ show (ctxLLM ctx)
    ++ ", ctxHistory = <IORef> }"

-- | Mutable state during workflow execution
data WorkflowState = WorkflowState
  { stCurrentPhase :: WorkflowPhase,
    stVariables :: [(Text, Value)],
    stStepCount :: Int,
    stActiveCapabilities :: [Capability]  -- Stack of active capabilities for current step
  }
  deriving (Generic)

-- | Capability definition
--   Natural language modifier for agent behavior
data Capability = Capability
  { capName :: Text,
    capDescription :: Text,
    capModifier :: Text -> Text, -- Simple prompt modifier for now
    capParameters :: Maybe Value
  }
  deriving (Generic)

instance Show Capability where
  show c = "Capability {capName = " ++ show (capName c) ++ "}"

-- | Errors that can occur during workflow execution
data WorkflowError
  = ToolNotFound Text
  | CapabilityMissing Text
  | TypeError Text Text -- Expected, Actual
  | TimeoutError Text
  | ValidationError Text
  | ExecutionError Text
  | ToolFailure ToolError
  deriving (Show, Eq, Generic)

-- | Serializable capability definition for JSON loading
--   This is the wire format that can be loaded from files
data CapabilityDef = CapabilityDef
  { capDefName :: Text,
    capDefDescription :: Text,
    capDefParameters :: Maybe Value,
    capDefModifierType :: Maybe Text, -- "prefix", "suffix", "wrap", "replace"
    capDefModifierValue :: Maybe Text -- The value used by the modifier
  }
  deriving (Show, Eq, Generic)
