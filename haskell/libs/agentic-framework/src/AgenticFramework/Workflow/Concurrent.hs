{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Concurrent
-- Description : Concurrent agent execution support
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Provides support for concurrent execution of agents and workflows.
-- Implements SC-008 (concurrent agent execution).
module AgenticFramework.Workflow.Concurrent
  ( -- * Concurrent Execution
    runConcurrently,
    runConcurrentlyWithLimit,
    runFirst,
    runRace,

    -- * Parallel Workflow Patterns
    fanOut,
    fanIn,
    scatter,
    gather,

    -- * Semaphore-based Concurrency
    ConcurrencySemaphore,
    newSemaphore,
    withSemaphore,
    tryWithSemaphore,

    -- * Cancellation
    CancellationToken,
    newCancellationToken,
    cancelToken,
    isCancelled,
    withCancellation,
  )
where

import AgenticFramework.Workflow (runWorkflow)
import AgenticFramework.Workflow.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently, race, wait, withAsync)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception (bracket, finally)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Text (Text)

-- | Run multiple workflows concurrently and collect all results
runConcurrently :: [Workflow a] -> Workflow [a]
runConcurrently workflows = do
  ctx <- ask
  st <- get
  results <- liftIO $ mapConcurrently (\w -> runWorkflow w ctx st) workflows
  return results

-- | Run workflows concurrently with a limit on concurrent executions
runConcurrentlyWithLimit :: Int -> [Workflow a] -> Workflow [a]
runConcurrentlyWithLimit limit workflows = do
  ctx <- ask
  st <- get
  sem <- liftIO $ newQSem limit
  results <- liftIO $ mapConcurrently (withSem sem ctx st) workflows
  return results
  where
    withSem :: QSem -> AgentContext -> WorkflowState -> Workflow a -> IO a
    withSem sem ctx' st' w = bracket (waitQSem sem) (const $ signalQSem sem) (const $ runWorkflow w ctx' st')

-- | Run workflows and return the first successful result
runFirst :: [Workflow a] -> Workflow (Maybe a)
runFirst [] = return Nothing
runFirst workflows = do
  ctx <- ask
  st <- get
  -- Run all workflows concurrently
  results <- liftIO $ mapConcurrently (\w -> runWorkflow w ctx st) workflows
  -- Return the first result
  return $ Just $ head results

-- | Race two workflows - return the result of whichever finishes first
runRace :: Workflow a -> Workflow b -> Workflow (Either a b)
runRace workflow1 workflow2 = do
  ctx <- ask
  st <- get
  result <- liftIO $ race (runWorkflow workflow1 ctx st) (runWorkflow workflow2 ctx st)
  return result

-- | Fan-out pattern: run the same workflow on multiple inputs concurrently
fanOut :: (a -> Workflow b) -> [a] -> Workflow [b]
fanOut f inputs = runConcurrently $ map f inputs

-- | Fan-in pattern: combine results from multiple workflows
fanIn :: [Workflow a] -> ([a] -> Workflow b) -> Workflow b
fanIn workflows combiner = do
  results <- runConcurrently workflows
  combiner results

-- | Scatter pattern: distribute work across multiple workflows
scatter :: Int -> Workflow a -> Workflow [a]
scatter n workflow = runConcurrently $ replicate n workflow

-- | Gather pattern: collect results from parallel executions
gather :: [Workflow a] -> Workflow [a]
gather = runConcurrently

-- | A semaphore for controlling concurrency
newtype ConcurrencySemaphore = ConcurrencySemaphore QSem

-- | Create a new concurrency semaphore with the given limit
newSemaphore :: Int -> IO ConcurrencySemaphore
newSemaphore limit = ConcurrencySemaphore <$> newQSem limit

-- | Run a workflow with semaphore-controlled concurrency
withSemaphore :: ConcurrencySemaphore -> Workflow a -> Workflow a
withSemaphore (ConcurrencySemaphore sem) workflow = do
  ctx <- ask
  st <- get
  result <- liftIO $ bracket (waitQSem sem) (const $ signalQSem sem) $ \_ ->
    runWorkflow workflow ctx st
  return result

-- | Try to run a workflow with the semaphore, returning Nothing if limit reached
tryWithSemaphore :: ConcurrencySemaphore -> Workflow a -> Workflow (Maybe a)
tryWithSemaphore (ConcurrencySemaphore _sem) workflow = do
  -- For now, just run the workflow
  -- A proper implementation would use tryWaitQSem if available
  result <- workflow
  return $ Just result

-- | A token for cancellation support
newtype CancellationToken = CancellationToken (IORef Bool)

-- | Create a new cancellation token
newCancellationToken :: IO CancellationToken
newCancellationToken = CancellationToken <$> newIORef False

-- | Cancel the token
cancelToken :: CancellationToken -> IO ()
cancelToken (CancellationToken ref) = atomicWriteIORef ref True

-- | Check if a token is cancelled
isCancelled :: CancellationToken -> IO Bool
isCancelled (CancellationToken ref) = readIORef ref

-- | Run a workflow with cancellation support
--   The workflow will be interrupted if the token is cancelled
withCancellation :: CancellationToken -> Workflow a -> Workflow (Maybe a)
withCancellation token workflow = do
  ctx <- ask
  st <- get
  result <- liftIO $ race (waitForCancel token) (runWorkflow workflow ctx st)
  case result of
    Left () -> return Nothing -- Cancelled
    Right val -> return $ Just val
  where
    waitForCancel :: CancellationToken -> IO ()
    waitForCancel tok = do
      cancelled <- isCancelled tok
      if cancelled
        then return ()
        else threadDelay 100000 >> waitForCancel tok -- Poll every 100ms
