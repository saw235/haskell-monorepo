{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Validation
-- Description : Workflow validation utilities
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.Validation
  ( -- * Validation Functions
    validateWorkflow,
    validateContext,
    validateCapabilities,
    validateTools,

    -- * CapabilityDef Validation (User Story 5)
    validateCapabilityDef,
    validateCapabilityDefs,
    CapabilityDefError (..),

    -- * Validation Types
    ValidationResult (..),
    ValidationError (..),
  )
where

import AgenticFramework.Types (Tool (..))
import AgenticFramework.Workflow.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Result of validation
data ValidationResult
  = Valid
  | Invalid [ValidationError]
  deriving (Show, Eq)

-- | Validation error types
data ValidationError
  = EmptySystemPrompt
  | EmptyUserPrompt
  | NoToolsAvailable
  | NoCapabilitiesAvailable
  | DuplicateToolName Text
  | DuplicateCapabilityName Text
  | InvalidLLMConfig Text
  | InvalidWorkflowState Text
  deriving (Show, Eq)

-- | Validate a complete workflow setup
--   Checks context, capabilities, and tools for consistency
validateWorkflow :: AgentContext -> WorkflowState -> ValidationResult
validateWorkflow ctx state =
  let contextErrors = validateContextErrors ctx
      stateErrors = validateStateErrors state
      allErrors = contextErrors ++ stateErrors
   in if null allErrors
        then Valid
        else Invalid allErrors

-- | Validate agent context
validateContext :: AgentContext -> ValidationResult
validateContext ctx =
  let errors = validateContextErrors ctx
   in if null errors
        then Valid
        else Invalid errors

-- | Internal: Get context validation errors
validateContextErrors :: AgentContext -> [ValidationError]
validateContextErrors ctx =
  concat
    [ checkSystemPrompt (ctxSystemPrompt ctx),
      checkUserPrompt (ctxUserPrompt ctx),
      checkTools (ctxTools ctx),
      checkCapabilities (ctxCapabilities ctx)
    ]

-- | Validate workflow state
validateStateErrors :: WorkflowState -> [ValidationError]
validateStateErrors state =
  [ InvalidWorkflowState "Step count is negative"
    | stStepCount state < 0
  ]

-- | Check system prompt is not empty
checkSystemPrompt :: Text -> [ValidationError]
checkSystemPrompt prompt
  | T.null prompt || T.all (== ' ') prompt = [EmptySystemPrompt]
  | otherwise = []

-- | Check user prompt is not empty
checkUserPrompt :: Text -> [ValidationError]
checkUserPrompt prompt
  | T.null prompt || T.all (== ' ') prompt = [EmptyUserPrompt]
  | otherwise = []

-- | Check tools for duplicates
checkTools :: [Tool] -> [ValidationError]
checkTools tools =
  let names = map toolName tools
      duplicates = findDuplicates names
   in map DuplicateToolName duplicates

-- | Check capabilities for duplicates
checkCapabilities :: [Capability] -> [ValidationError]
checkCapabilities caps =
  let names = map capName caps
      duplicates = findDuplicates names
   in map DuplicateCapabilityName duplicates

-- | Find duplicate elements in a list
findDuplicates :: (Eq a) => [a] -> [a]
findDuplicates [] = []
findDuplicates (x : xs)
  | x `elem` xs = x : findDuplicates (filter (/= x) xs)
  | otherwise = findDuplicates xs

-- | Validate capabilities are well-formed
validateCapabilities :: [Capability] -> ValidationResult
validateCapabilities caps =
  let errors = concatMap validateCapability caps
   in if null errors
        then Valid
        else Invalid errors

-- | Validate a single capability
validateCapability :: Capability -> [ValidationError]
validateCapability cap
  | T.null (capName cap) = [DuplicateCapabilityName ""]
  | T.null (capDescription cap) = [InvalidWorkflowState "Empty capability description"]
  | otherwise = []

-- | Validate tools are well-formed
validateTools :: [Tool] -> ValidationResult
validateTools tools =
  let errors = concatMap validateTool tools ++ checkTools tools
   in if null errors
        then Valid
        else Invalid errors

-- | Validate a single tool
validateTool :: Tool -> [ValidationError]
validateTool tool
  | T.null (toolName tool) = [DuplicateToolName ""]
  | T.null (toolDescription tool) = [InvalidWorkflowState "Empty tool description"]
  | otherwise = []

-- | Check if validation result is valid
isValid :: ValidationResult -> Bool
isValid Valid = True
isValid _ = False

-- | Check if validation result is invalid
isInvalid :: ValidationResult -> Bool
isInvalid = not . isValid

-- | Get errors from validation result
getErrors :: ValidationResult -> [ValidationError]
getErrors (Invalid errs) = errs
getErrors Valid = []

-- ============================================================================
-- CapabilityDef Validation (User Story 5 - FR-015, FR-011)
-- ============================================================================

-- | Errors specific to CapabilityDef validation
data CapabilityDefError
  = DefEmptyName
  | DefEmptyDescription
  | DefInvalidModifierType Text
  | DefMissingModifierValue Text -- Modifier type requires a value
  | DefDuplicateName Text
  deriving (Show, Eq)

-- | Validate a single CapabilityDef loaded from JSON
validateCapabilityDef :: CapabilityDef -> Either CapabilityDefError ()
validateCapabilityDef def
  | T.null (capDefName def) = Left DefEmptyName
  | T.null (capDefDescription def) = Left DefEmptyDescription
  | needsValue && noValue = Left $ DefMissingModifierValue modType
  | otherwise = Right ()
  where
    modType = maybe "" id (capDefModifierType def)
    needsValue = modType `elem` ["prefix", "suffix", "wrap", "replace"]
    noValue = case capDefModifierValue def of
      Nothing -> True
      Just v -> T.null v

-- | Validate a list of CapabilityDefs, checking for duplicates
validateCapabilityDefs :: [CapabilityDef] -> Either [CapabilityDefError] ()
validateCapabilityDefs defs =
  let individualErrors = map validateCapabilityDef defs
      validationErrors = [e | Left e <- individualErrors]
      names = map capDefName defs
      duplicateNames = findDuplicates names
      duplicateErrors = map DefDuplicateName duplicateNames
      allErrors = validationErrors ++ duplicateErrors
   in if null allErrors
        then Right ()
        else Left allErrors
