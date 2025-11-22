{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AgenticFramework.Capabilities where

import Control.Monad.Reader
import Control.Monad.State
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

-- | Approach 1: Phantom Types for Capability Tracking
-- Track what data the agent currently has access to
data AgentState = Empty | Observed | Searched | Reasoned

-- Phantom type parameter tracks the agent's current state
newtype AgentM (s :: AgentState) a = AgentM (ReaderT AgentEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- Operations that transform the agent's state
observe :: AgentM 'Empty Text -> AgentM 'Observed Text
observe (AgentM m) = AgentM $ do
  -- Can only observe when we have no data yet
  liftIO $ putStrLn "Observing environment..."
  m

search :: AgentM 'Observed Text -> AgentM 'Searched [Text]
search (AgentM m) = AgentM $ do
  -- Can only search after observing
  observation <- m
  liftIO $ putStrLn $ "Searching based on: " ++ show observation
  return ["result1", "result2"]

reason :: AgentM 'Searched [Text] -> AgentM 'Reasoned Text
reason (AgentM m) = AgentM $ do
  -- Can only reason after searching
  searchResults <- m
  liftIO $ putStrLn $ "Reasoning about: " ++ show searchResults
  return "conclusion"

respond :: AgentM 'Reasoned Text -> AgentM 'Empty Text
respond (AgentM m) = AgentM $ do
  -- Can only respond after reasoning
  reasoning <- m
  liftIO $ putStrLn $ "Responding with: " ++ show reasoning
  return reasoning

-- This compiles:
validFlow :: AgentM 'Empty Text -> AgentM 'Empty Text
validFlow initial =
  respond . reason . search . observe $ initial

-- This would NOT compile (type error):
-- invalidFlow :: AgentM 'Empty Text -> AgentM 'Empty Text
-- invalidFlow initial =
--   respond . observe . search . reason $ initial
--   -- Error: reason expects 'Searched but gets 'Reasoned

-- | Approach 2: Type Classes for Capabilities
-- More flexible: agents can have multiple capabilities
class HasObservation m where
  observeEnv :: m Text

class HasSearch m where
  searchFor :: Text -> m [Text]

class HasReasoning m where
  reasonAbout :: [Text] -> m Text

-- Now we can constrain functions to require certain capabilities:
analyzeEnvironment :: (HasObservation m, HasSearch m, HasReasoning m, Monad m) => m Text
analyzeEnvironment = do
  obs <- observeEnv
  results <- searchFor obs
  reasonAbout results

-- Different agents can have different capability sets:
newtype BasicAgent a = BasicAgent (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

newtype SmartAgent a = SmartAgent (StateT Context IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Context)

instance HasObservation BasicAgent where
  observeEnv = liftIO $ return "basic observation"

instance HasObservation SmartAgent where
  observeEnv = do
    ctx <- get
    liftIO $ return "smart observation with context"

-- Only SmartAgent has reasoning:
instance HasReasoning SmartAgent where
  reasonAbout data' = return $ "reasoned: " <> T.pack (show data')

-- | Approach 3: Indexed Monads for Protocol Safety
-- Ensures operations happen in valid sequences
data Phase = Start | ObservedPhase | SearchedPhase | ReasonedPhase | Done

-- IxMonad tracks phase transitions
class IxMonad (m :: Phase -> Phase -> Type -> Type) where
  ireturn :: a -> m i i a
  (>>>=) :: m i j a -> (a -> m j k b) -> m i k b

-- Agent operations that enforce phase transitions
newtype IxAgent (i :: Phase) (j :: Phase) a
  = IxAgent (ReaderT AgentEnv IO a)

instance IxMonad IxAgent where
  ireturn x = IxAgent (return x)
  (IxAgent m) >>>= f = IxAgent $ do
    a <- m
    let IxAgent m' = f a
    m'

-- Phase-specific operations
iobserve :: IxAgent 'Start 'ObservedPhase Text
iobserve = IxAgent $ do
  liftIO $ putStrLn "Observing..."
  return "observation"

isearch :: Text -> IxAgent 'ObservedPhase 'SearchedPhase [Text]
isearch query = IxAgent $ do
  liftIO $ putStrLn $ "Searching for: " ++ show query
  return ["result1", "result2"]

ireason :: [Text] -> IxAgent 'SearchedPhase 'ReasonedPhase Text
ireason results = IxAgent $ do
  liftIO $ putStrLn $ "Reasoning about: " ++ show results
  return "conclusion"

irespond :: Text -> IxAgent 'ReasonedPhase 'Done Text
irespond conclusion = IxAgent $ do
  liftIO $ putStrLn $ "Responding: " ++ show conclusion
  return conclusion

-- Valid flow compiles:
validProtocol :: IxAgent 'Start 'Done Text
validProtocol =
  iobserve >>>= \obs ->
    isearch obs >>>= \results ->
      ireason results >>>= \conclusion ->
        irespond conclusion

-- Invalid flow won't compile:
-- invalidProtocol :: IxAgent 'Start 'Done Text
-- invalidProtocol =
--   ireason [] >>>= \conclusion ->  -- Error: ireason needs 'SearchedPhase
--   iobserve >>>= \obs ->
--   irespond conclusion

-- | Approach 4: GADTs with Capability Requirements
-- Most explicit about what each operation needs
data Capability = NeedsNothing | NeedsObservation | NeedsSearchResults | NeedsReasoning

data AgentOp :: Capability -> Type -> Type where
  -- Observe requires nothing, produces observation
  Observe :: AgentOp 'NeedsNothing Text
  -- Search requires observation, produces results
  Search :: Text -> AgentOp 'NeedsObservation [Text]
  -- Reason requires search results, produces conclusion
  Reason :: [Text] -> AgentOp 'NeedsSearchResults Text
  -- Respond requires reasoning, produces response
  Respond :: Text -> AgentOp 'NeedsReasoning Text

-- Interpreter ensures capabilities are satisfied
runAgentOp :: AgentOp cap a -> IO a
runAgentOp Observe = do
  putStrLn "Observing environment..."
  return "observation"
runAgentOp (Search obs) = do
  putStrLn $ "Searching based on: " ++ show obs
  return ["result1", "result2"]
runAgentOp (Reason results) = do
  putStrLn $ "Reasoning about: " ++ show results
  return "conclusion"
runAgentOp (Respond conclusion) = do
  putStrLn $ "Responding with: " ++ show conclusion
  return conclusion

-- | Approach 5: Capability as Monad Transformer Stacks
-- Different stacks = different capabilities

-- Agent with observation capability (ReaderT for environment)
type ObservingAgent = ReaderT Environment IO

-- Agent with reasoning capability (StateT for context)
type ReasoningAgent = StateT ReasoningContext ObservingAgent

-- Agent with search capability (access to search index)
type SearchingAgent = ReaderT SearchIndex ReasoningAgent

-- Full agent has all capabilities composed
type FullAgent = SearchingAgent

-- Now operations naturally require the right transformer:
observeWithReader :: ObservingAgent Text
observeWithReader = do
  env <- ask
  return $ "observed: " <> T.pack (show env)

reasonWithState :: ReasoningAgent Text
reasonWithState = do
  obs <- lift observeWithReader -- Can still observe
  ctx <- get
  modify $ \c -> c {previousObservations = obs : previousObservations c}
  return "reasoned conclusion"

searchWithIndex :: SearchingAgent [Text]
searchWithIndex = do
  index <- ask -- Get search index
  conclusion <- lift reasonWithState -- Can reason
  -- Search using both index and reasoning
  return ["search results"]

-- | Helper Types
data AgentEnv = AgentEnv
  { agentTools :: [Text],
    agentConfig :: Text
  }

data Environment = Environment
  {envData :: Text}
  deriving (Show)

data ReasoningContext = ReasoningContext
  { previousObservations :: [Text],
    workingMemory :: [Text]
  }

data SearchIndex = SearchIndex
  {indexedData :: [Text]}

data Context = Context
  {ctxHistory :: [Text]}
