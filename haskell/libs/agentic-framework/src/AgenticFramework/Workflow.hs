{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow
-- Description : Workflow monad and execution
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow
  ( -- * Workflow Monad
    runWorkflow,
    runWorkflow_,

    -- * Default State
    defaultWorkflowState,

    -- * Types
    module AgenticFramework.Workflow.Types,
  )
where

import AgenticFramework.Workflow.Types
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)

-- | Run a workflow with the given context and initial state
runWorkflow :: Workflow a -> AgentContext -> WorkflowState -> IO a
runWorkflow (Workflow w) ctx st = evalStateT (runReaderT w ctx) st

-- | Run a workflow with the given context using default initial state
--   This is the simplified version for most use cases
runWorkflow_ :: Workflow a -> AgentContext -> IO a
runWorkflow_ workflow ctx = runWorkflow workflow ctx defaultWorkflowState

-- | Default initial state for workflow execution
--   Use this when you don't need custom state initialization
defaultWorkflowState :: WorkflowState
defaultWorkflowState =
  WorkflowState
    { stCurrentPhase = Executing,
      stVariables = [],
      stStepCount = 0,
      stActiveCapabilities = []
    }
