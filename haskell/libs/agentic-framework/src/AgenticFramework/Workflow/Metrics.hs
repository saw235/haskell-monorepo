{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Metrics
-- Description : Performance monitoring for workflows
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Provides performance metrics collection and reporting for workflow execution.
-- Implements SC-002 and SC-004 (performance requirements).
module AgenticFramework.Workflow.Metrics
  ( -- * Metrics Types
    WorkflowMetrics (..),
    StepMetrics (..),
    ExecutionStats (..),

    -- * Metrics Collection
    MetricsCollector,
    newMetricsCollector,
    recordStepStart,
    recordStepEnd,
    recordToolInvocation,
    recordLLMCall,

    -- * Metrics Retrieval
    getMetrics,
    getStepMetrics,
    getExecutionStats,

    -- * Metrics Reporting
    formatMetrics,
    formatMetricsJson,
    logMetrics,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

-- | Metrics for a single workflow step
data StepMetrics = StepMetrics
  { smStepId :: Int,
    smStepName :: Maybe Text,
    smStartTime :: UTCTime,
    smEndTime :: Maybe UTCTime,
    smDurationMs :: Maybe Double, -- Duration in milliseconds
    smToolCalls :: Int,
    smLLMCalls :: Int,
    smTokensUsed :: Int,
    smSuccess :: Bool
  }
  deriving (Show, Eq)

instance ToJSON StepMetrics where
  toJSON sm =
    object
      [ "step_id" .= smStepId sm,
        "step_name" .= smStepName sm,
        "duration_ms" .= smDurationMs sm,
        "tool_calls" .= smToolCalls sm,
        "llm_calls" .= smLLMCalls sm,
        "tokens_used" .= smTokensUsed sm,
        "success" .= smSuccess sm
      ]

-- | Overall metrics for a workflow execution
data WorkflowMetrics = WorkflowMetrics
  { wmWorkflowId :: Text,
    wmStartTime :: UTCTime,
    wmEndTime :: Maybe UTCTime,
    wmTotalDurationMs :: Maybe Double,
    wmStepMetrics :: [StepMetrics],
    wmTotalSteps :: Int,
    wmSuccessfulSteps :: Int,
    wmFailedSteps :: Int,
    wmTotalToolCalls :: Int,
    wmTotalLLMCalls :: Int,
    wmTotalTokens :: Int
  }
  deriving (Show, Eq)

instance ToJSON WorkflowMetrics where
  toJSON wm =
    object
      [ "workflow_id" .= wmWorkflowId wm,
        "total_duration_ms" .= wmTotalDurationMs wm,
        "total_steps" .= wmTotalSteps wm,
        "successful_steps" .= wmSuccessfulSteps wm,
        "failed_steps" .= wmFailedSteps wm,
        "total_tool_calls" .= wmTotalToolCalls wm,
        "total_llm_calls" .= wmTotalLLMCalls wm,
        "total_tokens" .= wmTotalTokens wm,
        "steps" .= wmStepMetrics wm
      ]

-- | Summary execution statistics
data ExecutionStats = ExecutionStats
  { esAverageStepDurationMs :: Double,
    esMaxStepDurationMs :: Double,
    esMinStepDurationMs :: Double,
    esP95StepDurationMs :: Double,
    esTotalOverheadMs :: Double, -- Time not in steps
    esStepSuccessRate :: Double -- 0.0 to 1.0
  }
  deriving (Show, Eq)

instance ToJSON ExecutionStats where
  toJSON es =
    object
      [ "avg_step_duration_ms" .= esAverageStepDurationMs es,
        "max_step_duration_ms" .= esMaxStepDurationMs es,
        "min_step_duration_ms" .= esMinStepDurationMs es,
        "p95_step_duration_ms" .= esP95StepDurationMs es,
        "total_overhead_ms" .= esTotalOverheadMs es,
        "step_success_rate" .= esStepSuccessRate es
      ]

-- | Internal state for metrics collection
data MetricsState = MetricsState
  { msWorkflowId :: Text,
    msStartTime :: UTCTime,
    msCurrentStep :: Maybe StepMetrics,
    msCompletedSteps :: [StepMetrics],
    msStepCounter :: Int
  }
  deriving (Show)

-- | Metrics collector handle
newtype MetricsCollector = MetricsCollector
  { unCollector :: MVar MetricsState
  }

-- | Create a new metrics collector for a workflow
newMetricsCollector :: Text -> IO MetricsCollector
newMetricsCollector workflowId = do
  now <- getCurrentTime
  let initialState =
        MetricsState
          { msWorkflowId = workflowId,
            msStartTime = now,
            msCurrentStep = Nothing,
            msCompletedSteps = [],
            msStepCounter = 0
          }
  mvar <- newMVar initialState
  return $ MetricsCollector mvar

-- | Record the start of a workflow step
recordStepStart :: MetricsCollector -> Maybe Text -> IO ()
recordStepStart (MetricsCollector mvar) stepName = do
  now <- getCurrentTime
  modifyMVar_ mvar $ \state -> do
    let stepId = msStepCounter state + 1
        newStep =
          StepMetrics
            { smStepId = stepId,
              smStepName = stepName,
              smStartTime = now,
              smEndTime = Nothing,
              smDurationMs = Nothing,
              smToolCalls = 0,
              smLLMCalls = 0,
              smTokensUsed = 0,
              smSuccess = False
            }
    return $
      state
        { msCurrentStep = Just newStep,
          msStepCounter = stepId
        }

-- | Record the end of a workflow step
recordStepEnd :: MetricsCollector -> Bool -> IO ()
recordStepEnd (MetricsCollector mvar) success = do
  now <- getCurrentTime
  modifyMVar_ mvar $ \state ->
    case msCurrentStep state of
      Nothing -> return state -- No step in progress
      Just step -> do
        let duration = diffToMs $ diffUTCTime now (smStartTime step)
            completedStep =
              step
                { smEndTime = Just now,
                  smDurationMs = Just duration,
                  smSuccess = success
                }
        return $
          state
            { msCurrentStep = Nothing,
              msCompletedSteps = msCompletedSteps state ++ [completedStep]
            }

-- | Record a tool invocation within the current step
recordToolInvocation :: MetricsCollector -> IO ()
recordToolInvocation (MetricsCollector mvar) =
  modifyMVar_ mvar $ \state ->
    case msCurrentStep state of
      Nothing -> return state
      Just step ->
        return $
          state
            { msCurrentStep =
                Just $ step {smToolCalls = smToolCalls step + 1}
            }

-- | Record an LLM call within the current step
recordLLMCall :: MetricsCollector -> Int -> IO ()
recordLLMCall (MetricsCollector mvar) tokens =
  modifyMVar_ mvar $ \state ->
    case msCurrentStep state of
      Nothing -> return state
      Just step ->
        return $
          state
            { msCurrentStep =
                Just $
                  step
                    { smLLMCalls = smLLMCalls step + 1,
                      smTokensUsed = smTokensUsed step + tokens
                    }
            }

-- | Get the current workflow metrics
getMetrics :: MetricsCollector -> IO WorkflowMetrics
getMetrics (MetricsCollector mvar) = do
  now <- getCurrentTime
  state <- readMVar mvar
  let steps = msCompletedSteps state
      duration = diffToMs $ diffUTCTime now (msStartTime state)
  return $
    WorkflowMetrics
      { wmWorkflowId = msWorkflowId state,
        wmStartTime = msStartTime state,
        wmEndTime = Just now,
        wmTotalDurationMs = Just duration,
        wmStepMetrics = steps,
        wmTotalSteps = length steps,
        wmSuccessfulSteps = length $ filter smSuccess steps,
        wmFailedSteps = length $ filter (not . smSuccess) steps,
        wmTotalToolCalls = sum $ map smToolCalls steps,
        wmTotalLLMCalls = sum $ map smLLMCalls steps,
        wmTotalTokens = sum $ map smTokensUsed steps
      }

-- | Get metrics for a specific step
getStepMetrics :: MetricsCollector -> Int -> IO (Maybe StepMetrics)
getStepMetrics (MetricsCollector mvar) stepId = do
  state <- readMVar mvar
  return $ findStep stepId (msCompletedSteps state)
  where
    findStep sid steps = case filter ((== sid) . smStepId) steps of
      (s : _) -> Just s
      [] -> Nothing

-- | Calculate execution statistics
getExecutionStats :: MetricsCollector -> IO ExecutionStats
getExecutionStats collector = do
  metrics <- getMetrics collector
  let durations =
        [ d
          | step <- wmStepMetrics metrics,
            Just d <- [smDurationMs step]
        ]
      sortedDurations = quickSort durations
      n = length sortedDurations
  return $
    ExecutionStats
      { esAverageStepDurationMs = if n == 0 then 0 else sum durations / fromIntegral n,
        esMaxStepDurationMs = if null durations then 0 else maximum durations,
        esMinStepDurationMs = if null durations then 0 else minimum durations,
        esP95StepDurationMs = percentile 95 sortedDurations,
        esTotalOverheadMs = maybe 0 id (wmTotalDurationMs metrics) - sum durations,
        esStepSuccessRate =
          if wmTotalSteps metrics == 0
            then 1.0
            else fromIntegral (wmSuccessfulSteps metrics) / fromIntegral (wmTotalSteps metrics)
      }

-- | Format metrics as human-readable text
formatMetrics :: WorkflowMetrics -> Text
formatMetrics wm =
  T.unlines
    [ "Workflow: " <> wmWorkflowId wm,
      "Duration: " <> maybe "N/A" showMs (wmTotalDurationMs wm),
      "Steps: "
        <> T.pack (show (wmTotalSteps wm))
        <> " ("
        <> T.pack (show (wmSuccessfulSteps wm))
        <> " successful, "
        <> T.pack (show (wmFailedSteps wm))
        <> " failed)",
      "Tool calls: " <> T.pack (show (wmTotalToolCalls wm)),
      "LLM calls: " <> T.pack (show (wmTotalLLMCalls wm)),
      "Tokens used: " <> T.pack (show (wmTotalTokens wm))
    ]
  where
    showMs ms = T.pack (show ms) <> "ms"

-- | Format metrics as JSON
formatMetricsJson :: WorkflowMetrics -> Text
formatMetricsJson wm = T.pack $ show $ Aeson.encode wm

-- | Log metrics (placeholder - would integrate with logging system)
logMetrics :: WorkflowMetrics -> IO ()
logMetrics wm = putStrLn $ T.unpack $ formatMetrics wm

-- | Convert NominalDiffTime to milliseconds
diffToMs :: NominalDiffTime -> Double
diffToMs dt = realToFrac dt * 1000

-- | Simple quicksort for percentile calculation
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = filter (< x) xs
    larger = filter (>= x) xs

-- | Calculate percentile from sorted list
percentile :: Double -> [Double] -> Double
percentile _ [] = 0
percentile p sorted =
  let n = length sorted
      idx = floor $ (p / 100) * fromIntegral (n - 1)
   in sorted !! idx
