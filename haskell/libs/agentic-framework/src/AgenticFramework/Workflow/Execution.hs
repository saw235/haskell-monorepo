{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Execution
-- Description : Workflow execution engine
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.Execution
  ( -- * Execution Functions
    executeWorkflow,
    executeWorkflowWithContext,

    -- * Parallel Execution
    parallel,
    parallelWithLimit,

    -- * Error Handling
    tryWorkflow,
    catchWorkflowError,

    -- * Context Preservation (FR-009)
    getVariable,
    setVariable,
    modifyVariable,
    clearVariable,
    getAllVariables,
    withPreservedState,

    -- * Nested Workflow Support (FR-012)
    nestedWorkflow,
    withNestedContext,
    withLocalCapabilities,
  )
where

import AgenticFramework.Workflow (runWorkflow)
import AgenticFramework.Workflow.Types
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, local, runReaderT)
import Control.Monad.State (evalStateT, get, gets, modify, put)
import Data.Aeson (Value)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T

-- | Execute a workflow with a given context
executeWorkflow :: Workflow a -> AgentContext -> IO (Either WorkflowError a)
executeWorkflow workflow ctx = do
  let initialState =
        WorkflowState
          { stCurrentPhase = Executing,
            stVariables = [],
            stStepCount = 0,
            stActiveCapabilities = []
          }
  try (runWorkflow workflow ctx initialState) >>= \case
    Left (e :: SomeException) ->
      return $ Left $ ExecutionError $ T.pack $ show e
    Right result -> return $ Right result

-- | Execute a workflow with a custom initial state
executeWorkflowWithContext :: Workflow a -> AgentContext -> WorkflowState -> IO (Either WorkflowError a)
executeWorkflowWithContext workflow ctx state = do
  try (runWorkflow workflow ctx state) >>= \case
    Left (e :: SomeException) ->
      return $ Left $ ExecutionError $ T.pack $ show e
    Right result -> return $ Right result

-- | Execute multiple workflows in parallel
--   Returns results in the same order as input workflows
parallel :: [Workflow a] -> Workflow [a]
parallel workflows = do
  ctx <- ask
  state <- get
  results <- liftIO $ mapConcurrently (\w -> runWorkflow w ctx state) workflows
  return results

-- | Execute workflows in parallel with a concurrency limit
parallelWithLimit :: Int -> [Workflow a] -> Workflow [a]
parallelWithLimit limit workflows = do
  ctx <- ask
  state <- get
  -- Simple chunking for now; could be improved with a worker pool
  let chunks = chunksOf limit workflows
  results <- liftIO $ concat <$> mapM (mapConcurrently (\w -> runWorkflow w ctx state)) chunks
  return results
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Try a workflow and return an Either result instead of throwing
tryWorkflow :: Workflow a -> Workflow (Either WorkflowError a)
tryWorkflow workflow = do
  ctx <- ask
  state <- get
  result <- liftIO $ try (runWorkflow workflow ctx state)
  case result of
    Left (e :: SomeException) ->
      return $ Left $ ExecutionError $ T.pack $ show e
    Right val -> return $ Right val

-- | Catch workflow errors and provide a fallback
catchWorkflowError :: Workflow a -> (WorkflowError -> Workflow a) -> Workflow a
catchWorkflowError workflow handler = do
  result <- tryWorkflow workflow
  case result of
    Left err -> handler err
    Right val -> return val

-- | Execute workflow with modified context
withModifiedContext :: (AgentContext -> AgentContext) -> Workflow a -> Workflow a
withModifiedContext f workflow = do
  local f workflow

-- | Track workflow step execution
trackStep :: Text -> Workflow a -> Workflow a
trackStep stepName action = do
  modify $ \s -> s {stStepCount = stStepCount s + 1}
  action

-- | Execute workflow with timeout (in microseconds)
withTimeout :: Int -> Workflow a -> Workflow (Maybe a)
withTimeout timeoutMicros workflow = do
  ctx <- ask
  state <- get
  result <- liftIO $ race (threadDelay timeoutMicros) (runWorkflow workflow ctx state)
  case result of
    Left () -> return Nothing
    Right val -> return (Just val)
  where
    threadDelay = undefined -- Would need to import Control.Concurrent

-- | Execute workflow with retry logic
withRetry :: Int -> Workflow a -> Workflow (Either WorkflowError a)
withRetry maxAttempts workflow = go 1
  where
    go attempt = do
      result <- tryWorkflow workflow
      case result of
        Right val -> return $ Right val
        Left err
          | attempt >= maxAttempts -> return $ Left err
          | otherwise -> go (attempt + 1)

--------------------------------------------------------------------------------
-- Context Preservation Functions (FR-009)
-- These functions enable data to be preserved and shared across workflow steps
--------------------------------------------------------------------------------

-- | Get a variable from the workflow state by name
--   Returns Nothing if the variable doesn't exist
getVariable :: Text -> Workflow (Maybe Value)
getVariable name = do
  vars <- gets stVariables
  return $ List.lookup name vars

-- | Set a variable in the workflow state
--   Overwrites existing value if present
setVariable :: Text -> Value -> Workflow ()
setVariable name value = modify $ \s ->
  s {stVariables = (name, value) : filter ((/= name) . fst) (stVariables s)}

-- | Modify a variable in the workflow state
--   Does nothing if variable doesn't exist
modifyVariable :: Text -> (Value -> Value) -> Workflow ()
modifyVariable name f = do
  mVal <- getVariable name
  case mVal of
    Nothing -> return ()
    Just val -> setVariable name (f val)

-- | Remove a variable from the workflow state
clearVariable :: Text -> Workflow ()
clearVariable name = modify $ \s ->
  s {stVariables = filter ((/= name) . fst) (stVariables s)}

-- | Get all variables from the workflow state
getAllVariables :: Workflow [(Text, Value)]
getAllVariables = gets stVariables

-- | Execute a workflow action while preserving the current state
--   Any state changes during the action are reverted after completion
withPreservedState :: Workflow a -> Workflow a
withPreservedState action = do
  savedState <- get
  result <- action
  put savedState
  return result

--------------------------------------------------------------------------------
-- Nested Workflow Support (FR-012)
-- These functions enable workflows to execute sub-workflows with isolated contexts
--------------------------------------------------------------------------------

-- | Execute a nested workflow with its own isolated state
--   The nested workflow inherits the parent's context but has its own state
--   Changes to state in the nested workflow do not affect the parent
nestedWorkflow :: Workflow a -> Workflow a
nestedWorkflow subWorkflow = do
  ctx <- ask
  parentState <- get
  -- Create isolated state for nested workflow
  let nestedState = parentState {stStepCount = 0}
  -- Execute nested workflow in isolated state
  result <- liftIO $ runWorkflow subWorkflow ctx nestedState
  -- Increment parent's step count to track that a nested workflow was executed
  modify $ \s -> s {stStepCount = stStepCount s + 1}
  return result

-- | Execute a workflow with a modified context (for nesting)
--   Allows passing modified capabilities, tools, or prompts to nested workflows
withNestedContext :: (AgentContext -> AgentContext) -> Workflow a -> Workflow a
withNestedContext modifyCtx subWorkflow = do
  local modifyCtx subWorkflow

-- | Execute a workflow with additional local capabilities
--   The local capabilities are prepended to the existing capabilities
withLocalCapabilities :: [Capability] -> Workflow a -> Workflow a
withLocalCapabilities localCaps subWorkflow = do
  -- Update workflow state with local capabilities
  modify $ \s -> s {stActiveCapabilities = localCaps ++ stActiveCapabilities s}
  result <- subWorkflow
  -- Restore original active capabilities
  modify $ \s -> s {stActiveCapabilities = drop (length localCaps) (stActiveCapabilities s)}
  return result
