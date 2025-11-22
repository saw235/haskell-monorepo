{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : AgenticFramework.Workflow.Indexed
-- Description : Indexed monad for type-safe workflow phases
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides an indexed monad implementation for workflows,
-- allowing compile-time verification of workflow phase transitions.
module AgenticFramework.Workflow.Indexed
  ( -- * Indexed Workflow Types
    IWorkflow (..),
    WorkflowPhase (..),

    -- * Phase Transitions
    initialize,
    configure,
    ready,
    execute,
    complete,

    -- * Conversion
    toWorkflow,
    fromWorkflow,
  )
where

import AgenticFramework.Workflow.Types (AgentContext, Workflow (..), WorkflowPhase (..), WorkflowState (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, modify, put)

-- | Indexed workflow monad that tracks phase transitions at the type level
--   The type parameters 'i' and 'j' represent the input and output phases
newtype IWorkflow (i :: WorkflowPhase) (j :: WorkflowPhase) a = IWorkflow
  { runIWorkflow :: ReaderT AgentContext (StateT WorkflowState IO) a
  }

-- | Functor instance for indexed workflows
instance Functor (IWorkflow i j) where
  fmap f (IWorkflow w) = IWorkflow (fmap f w)

-- | Applicative instance for indexed workflows (same phase)
instance Applicative (IWorkflow i i) where
  pure x = IWorkflow (pure x)
  IWorkflow wf <*> IWorkflow wx = IWorkflow (wf <*> wx)

-- | Monad instance for indexed workflows (same phase)
instance Monad (IWorkflow i i) where
  IWorkflow w >>= f = IWorkflow (w >>= runIWorkflow . f)

-- | MonadIO instance for indexed workflows
instance MonadIO (IWorkflow i i) where
  liftIO action = IWorkflow (liftIO action)

-- | Compose indexed workflows with phase transitions
--   This is the key operation that allows chaining workflows through phases
(>>>=) :: IWorkflow i j a -> (a -> IWorkflow j k b) -> IWorkflow i k b
IWorkflow w1 >>>= f = IWorkflow $ do
  a <- w1
  runIWorkflow (f a)

-- | Initialize workflow (transition from any state to Init)
initialize :: IWorkflow i 'Init ()
initialize = IWorkflow $ do
  modify $ \s -> s {stCurrentPhase = Init}

-- | Configure workflow (transition from Init to Configured)
configure :: a -> IWorkflow 'Init 'Configured a
configure config = IWorkflow $ do
  modify $ \s -> s {stCurrentPhase = Configured}
  return config

-- | Mark workflow as ready (transition from Configured to Ready)
ready :: IWorkflow 'Configured 'Ready ()
ready = IWorkflow $ do
  modify $ \s -> s {stCurrentPhase = Ready}

-- | Execute workflow step (transition from Ready to Executing)
execute :: IWorkflow 'Ready 'Executing a -> IWorkflow 'Ready 'Executing a
execute (IWorkflow action) = IWorkflow $ do
  modify $ \s -> s {stCurrentPhase = Executing}
  action

-- | Complete workflow (transition from Executing to Complete)
complete :: a -> IWorkflow 'Executing 'Complete a
complete result = IWorkflow $ do
  modify $ \s -> s {stCurrentPhase = Complete}
  return result

-- | Convert indexed workflow to regular workflow
toWorkflow :: IWorkflow i j a -> Workflow a
toWorkflow (IWorkflow w) = Workflow w

-- | Convert regular workflow to indexed workflow (same phase)
fromWorkflow :: Workflow a -> IWorkflow i i a
fromWorkflow (Workflow w) = IWorkflow w

-- | Lift a workflow action that doesn't change phase
liftPhase :: Workflow a -> IWorkflow i i a
liftPhase (Workflow w) = IWorkflow w

-- | Example: Type-safe workflow construction
--
-- @
-- safeWorkflow :: IWorkflow 'Init 'Complete Text
-- safeWorkflow =
--   initialize >>>=
--   \_ -> configure "config" >>>=
--   \cfg -> ready >>>=
--   \_ -> execute (liftPhase $ getUserPrompt) >>>=
--   \prompt -> complete prompt
-- @
