{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Errors
-- Description : Comprehensive error handling and messages
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Provides detailed, user-friendly error messages for workflow execution.
-- Implements FR-011 (error handling) and SC-007 (clear error messages).
module AgenticFramework.Workflow.Errors
  ( -- * Error Types
    DetailedError (..),
    ErrorContext (..),
    ErrorSeverity (..),

    -- * Error Construction
    makeDetailedError,
    withErrorContext,

    -- * Error Formatting
    formatError,
    formatErrorPlain,
    formatErrorJson,
    formatErrorForLog,

    -- * Error Suggestions
    suggestFix,
    commonFixes,

    -- * Error Categories
    categorizeError,
    ErrorCategory (..),
  )
where

import AgenticFramework.Workflow.Types (WorkflowError (..))
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | Error severity levels
data ErrorSeverity
  = SeverityInfo -- Informational, not an error
  | SeverityWarning -- Warning, workflow can continue
  | SeverityError -- Error, workflow step failed
  | SeverityCritical -- Critical, workflow cannot continue
  deriving (Show, Eq, Ord)

-- | Error categories for grouping related errors
data ErrorCategory
  = CategoryConfiguration -- Configuration or setup issues
  | CategoryValidation -- Input validation errors
  | CategoryExecution -- Runtime execution errors
  | CategoryTool -- Tool-related errors
  | CategoryCapability -- Capability-related errors
  | CategoryTimeout -- Timeout errors
  | CategoryInternal -- Internal/unexpected errors
  deriving (Show, Eq)

-- | Context information for an error
data ErrorContext = ErrorContext
  { ecWorkflowStep :: Maybe Int,
    ecAgentId :: Maybe Text,
    ecToolName :: Maybe Text,
    ecCapabilityName :: Maybe Text,
    ecInput :: Maybe Text,
    ecTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | A detailed error with context and suggestions
data DetailedError = DetailedError
  { deOriginalError :: WorkflowError,
    deSeverity :: ErrorSeverity,
    deCategory :: ErrorCategory,
    deContext :: ErrorContext,
    deMessage :: Text, -- Human-readable message
    deSuggestions :: [Text], -- Possible fixes
    deTechnicalDetails :: Maybe Text -- For debugging
  }
  deriving (Show, Eq)

instance ToJSON DetailedError where
  toJSON err =
    object
      [ "error" .= show (deOriginalError err),
        "severity" .= show (deSeverity err),
        "category" .= show (deCategory err),
        "message" .= deMessage err,
        "suggestions" .= deSuggestions err,
        "technical_details" .= deTechnicalDetails err,
        "context"
          .= object
            [ "step" .= ecWorkflowStep (deContext err),
              "agent_id" .= ecAgentId (deContext err),
              "tool_name" .= ecToolName (deContext err),
              "capability_name" .= ecCapabilityName (deContext err)
            ]
      ]

-- | Create an empty error context
emptyContext :: ErrorContext
emptyContext =
  ErrorContext
    { ecWorkflowStep = Nothing,
      ecAgentId = Nothing,
      ecToolName = Nothing,
      ecCapabilityName = Nothing,
      ecInput = Nothing,
      ecTimestamp = Nothing
    }

-- | Create a detailed error from a workflow error
makeDetailedError :: WorkflowError -> DetailedError
makeDetailedError err =
  DetailedError
    { deOriginalError = err,
      deSeverity = errorSeverity err,
      deCategory = categorizeError err,
      deContext = emptyContext,
      deMessage = errorMessage err,
      deSuggestions = suggestFix err,
      deTechnicalDetails = Nothing
    }

-- | Add context to a detailed error
withErrorContext :: ErrorContext -> DetailedError -> DetailedError
withErrorContext ctx err = err {deContext = ctx}

-- | Get the severity of a workflow error
errorSeverity :: WorkflowError -> ErrorSeverity
errorSeverity (ToolNotFound _) = SeverityError
errorSeverity (CapabilityMissing _) = SeverityError
errorSeverity (TypeError _ _) = SeverityError
errorSeverity (TimeoutError _) = SeverityWarning
errorSeverity (ValidationError _) = SeverityError
errorSeverity (ExecutionError _) = SeverityCritical
errorSeverity (ToolFailure _) = SeverityError

-- | Categorize a workflow error
categorizeError :: WorkflowError -> ErrorCategory
categorizeError (ToolNotFound _) = CategoryTool
categorizeError (CapabilityMissing _) = CategoryCapability
categorizeError (TypeError _ _) = CategoryValidation
categorizeError (TimeoutError _) = CategoryTimeout
categorizeError (ValidationError _) = CategoryValidation
categorizeError (ExecutionError _) = CategoryExecution
categorizeError (ToolFailure _) = CategoryTool

-- | Generate a human-readable error message
errorMessage :: WorkflowError -> Text
errorMessage (ToolNotFound name) =
  "The tool '" <> name <> "' was not found. It may not be registered with this agent or may be misspelled."
errorMessage (CapabilityMissing name) =
  "The capability '" <> name <> "' is required but not available. Please ensure it is loaded or defined."
errorMessage (TypeError expected actual) =
  "Type mismatch: expected " <> expected <> " but received " <> actual <> ". Check the data flow between workflow steps."
errorMessage (TimeoutError operation) =
  "The operation '" <> operation <> "' timed out. This may be due to network issues or an unresponsive external service."
errorMessage (ValidationError details) =
  "Validation failed: " <> details
errorMessage (ExecutionError details) =
  "Execution error: " <> details <> ". This may indicate a bug or unexpected state."
errorMessage (ToolFailure toolErr) =
  "Tool execution failed: " <> T.pack (show toolErr)

-- | Suggest fixes for a workflow error
suggestFix :: WorkflowError -> [Text]
suggestFix (ToolNotFound name) =
  [ "Check that the tool '" <> name <> "' is correctly registered with the agent",
    "Verify the tool name is spelled correctly (names are case-sensitive)",
    "Ensure the tool is included in the agent's tool list during creation"
  ]
suggestFix (CapabilityMissing name) =
  [ "Load the '" <> name <> "' capability using loadCapability or withCapability",
    "Check that the capability JSON file exists in the capabilities directory",
    "Verify the capability name matches the 'name' field in the JSON definition"
  ]
suggestFix (TypeError expected actual) =
  [ "Check that the previous workflow step returns the expected type",
    "Use type annotations to help identify the mismatch location",
    "Consider using a type conversion function between steps"
  ]
suggestFix (TimeoutError _) =
  [ "Increase the timeout duration for this operation",
    "Check network connectivity and external service availability",
    "Consider breaking the operation into smaller steps"
  ]
suggestFix (ValidationError _) =
  [ "Review the input data for missing or invalid fields",
    "Check that all required fields are provided",
    "Ensure data types match the expected schema"
  ]
suggestFix (ExecutionError _) =
  [ "Check the execution logs for more details",
    "Verify all dependencies are correctly initialized",
    "Report this issue if it persists"
  ]
suggestFix (ToolFailure _) =
  [ "Check the tool's input parameters",
    "Verify the tool has the required permissions",
    "Check the tool's execution logs for details"
  ]

-- | Common fixes that apply to many errors
commonFixes :: [Text]
commonFixes =
  [ "Restart the agent to clear any stale state",
    "Check the agent's configuration for issues",
    "Review the workflow definition for logical errors"
  ]

-- | Format an error for display (with colors for terminal)
formatError :: DetailedError -> Text
formatError err =
  severityPrefix (deSeverity err)
    <> " "
    <> deMessage err
    <> "\n"
    <> formatSuggestions (deSuggestions err)
    <> formatContext (deContext err)
  where
    severityPrefix SeverityInfo = "[INFO]"
    severityPrefix SeverityWarning = "[WARN]"
    severityPrefix SeverityError = "[ERROR]"
    severityPrefix SeverityCritical = "[CRITICAL]"

    formatSuggestions [] = ""
    formatSuggestions suggs =
      "\nSuggested fixes:\n" <> T.unlines (map ("  - " <>) suggs)

    formatContext ctx =
      let parts =
            catMaybes
              [ ("Step: " <>) . T.pack . show <$> ecWorkflowStep ctx,
                ("Tool: " <>) <$> ecToolName ctx,
                ("Capability: " <>) <$> ecCapabilityName ctx
              ]
       in if null parts
            then ""
            else "\nContext:\n" <> T.unlines (map ("  " <>) parts)

-- | Filter Nothing values from a list
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\mx acc -> maybe acc (: acc) mx) []

-- | Format an error as plain text (no formatting)
formatErrorPlain :: DetailedError -> Text
formatErrorPlain err =
  "Error: "
    <> deMessage err
    <> maybe "" ("\n\nTechnical details: " <>) (deTechnicalDetails err)

-- | Format an error as JSON
formatErrorJson :: DetailedError -> Text
formatErrorJson err =
  T.pack $ show $ Aeson.encode err

-- | Format an error for logging
formatErrorForLog :: DetailedError -> Text
formatErrorForLog err =
  T.intercalate
    " | "
    [ "[" <> T.pack (show (deSeverity err)) <> "]",
      "[" <> T.pack (show (deCategory err)) <> "]",
      T.pack (show (deOriginalError err)),
      deMessage err
    ]
