{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Reasoning
-- Description : Formal logical reasoning workflows using modus ponens and modus tollens
-- Copyright   : (c) 2025
-- License     : MIT
--
-- This module provides workflows for formal logical reasoning.
-- The LLM is instructed to produce logically valid and sound arguments
-- using classical inference rules:
--
-- * Modus Ponens: If P then Q. P. Therefore Q.
-- * Modus Tollens: If P then Q. Not Q. Therefore not P.
--
-- The output is structured to show premises, inference rule, and conclusion.
module AgenticFramework.Workflow.Reasoning
  ( -- * Reasoning Types
    Argument (..),
    Premise (..),
    Conclusion (..),
    InferenceRule (..),
    ReasoningResult (..),

    -- * Reasoning Workflows
    reasoningWorkflow,
    modusPonensWorkflow,
    modusTollensWorkflow,

    -- * Parsing
    parseReasoningResult,

    -- * Prompts
    formalReasoningPrompt,
    modusPonensPrompt,
    modusTollensPrompt,
  )
where

import AgenticFramework.Workflow.DSL (getUserPrompt, llmCall)
import AgenticFramework.Workflow.Types (Workflow)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- ============================================================================
-- Types for Formal Reasoning
-- ============================================================================

-- | A premise in a logical argument
data Premise = Premise
  { premiseId :: Int,
    premiseText :: Text,
    premiseType :: PremiseType
  }
  deriving (Show, Eq, Generic)

-- | Type of premise
data PremiseType
  = -- | "If P then Q" (P → Q)
    Conditional
  | -- | "P is true" (P)
    Affirmation
  | -- | "Q is false" (¬Q)
    Negation
  | -- | Raw observation from input
    Observation
  deriving (Show, Eq, Generic)

-- | The conclusion of an argument
data Conclusion = Conclusion
  { conclusionText :: Text,
    conclusionValidity :: Validity
  }
  deriving (Show, Eq, Generic)

-- | Whether the conclusion follows validly
data Validity
  = -- | Conclusion follows necessarily from premises
    Valid
  | -- | Logical fallacy detected
    Invalid
  | -- | Cannot determine validity
    Uncertain
  deriving (Show, Eq, Generic)

-- | Classical inference rules
data InferenceRule
  = -- | P → Q, P ⊢ Q
    ModusPonens
  | -- | P → Q, ¬Q ⊢ ¬P
    ModusTollens
  | -- | P → Q, Q → R, P ⊢ R (hypothetical syllogism + MP)
    ModusPonensChain
  | -- | P ∨ Q, ¬P ⊢ Q
    DisjunctiveSyllogism
  | -- | No valid inference possible
    NoInference
  deriving (Show, Eq, Generic)

-- | A complete logical argument
data Argument = Argument
  { argPremises :: [Premise],
    argRule :: InferenceRule,
    argConclusion :: Conclusion
  }
  deriving (Show, Eq, Generic)

-- | Result of the reasoning workflow
data ReasoningResult = ReasoningResult
  { -- | Original observations
    resObservations :: Text,
    -- | The formal argument
    resArgument :: Argument,
    -- | Natural language explanation
    resExplanation :: Text,
    -- | Soundness: premises are actually true
    resIsSound :: Bool,
    -- | Validity: conclusion follows from premises
    resIsValid :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Premise

instance FromJSON Premise

instance ToJSON PremiseType

instance FromJSON PremiseType

instance ToJSON Conclusion

instance FromJSON Conclusion

instance ToJSON Validity

instance FromJSON Validity

instance ToJSON InferenceRule

instance FromJSON InferenceRule

instance ToJSON Argument

instance FromJSON Argument

instance ToJSON ReasoningResult

instance FromJSON ReasoningResult

-- ============================================================================
-- Prompts for Formal Reasoning
-- ============================================================================

-- | System prompt for formal logical reasoning
formalReasoningPrompt :: Text
formalReasoningPrompt =
  T.unlines
    [ "You are a formal logic assistant. Given observations, construct a logically VALID and SOUND argument.",
      "",
      "VALIDITY: The conclusion MUST follow necessarily from the premises using proper inference rules.",
      "SOUNDNESS: The premises MUST be true based on the given observations.",
      "",
      "Available inference rules:",
      "1. MODUS PONENS: If P then Q. P is true. Therefore, Q is true.",
      "   - Symbolically: P → Q, P ⊢ Q",
      "",
      "2. MODUS TOLLENS: If P then Q. Q is false. Therefore, P is false.",
      "   - Symbolically: P → Q, ¬Q ⊢ ¬P",
      "",
      "RESPOND IN THIS EXACT FORMAT:",
      "---",
      "OBSERVATIONS:",
      "[Restate the given observations]",
      "",
      "PREMISES:",
      "P1 [CONDITIONAL]: If [antecedent] then [consequent]",
      "P2 [AFFIRMATION|NEGATION]: [statement]",
      "",
      "INFERENCE RULE: [MODUS_PONENS|MODUS_TOLLENS]",
      "",
      "CONCLUSION:",
      "[The logically derived conclusion]",
      "",
      "VALIDITY: [VALID|INVALID]",
      "SOUNDNESS: [SOUND|UNSOUND]",
      "",
      "EXPLANATION:",
      "[Brief explanation of why this inference is valid and sound]",
      "---",
      "",
      "IMPORTANT:",
      "- Do NOT affirm the consequent (P → Q, Q ⊢ P is INVALID)",
      "- Do NOT deny the antecedent (P → Q, ¬P ⊢ ¬Q is INVALID)",
      "- Only draw conclusions that NECESSARILY follow",
      "- If no valid inference is possible, state 'NO_VALID_INFERENCE'"
    ]

-- | Prompt specifically for modus ponens reasoning
modusPonensPrompt :: Text
modusPonensPrompt =
  T.unlines
    [ "You are a formal logic assistant. Apply MODUS PONENS to the given observations.",
      "",
      "MODUS PONENS (Affirming the Antecedent):",
      "  Premise 1: If P then Q  (P → Q)",
      "  Premise 2: P is true    (P)",
      "  Conclusion: Q is true   (Q)",
      "",
      "Your task:",
      "1. Identify a conditional statement (If P then Q) from the observations",
      "2. Identify an affirmation of the antecedent (P is true)",
      "3. Derive the conclusion (Q is true)",
      "",
      "RESPOND IN THIS EXACT FORMAT:",
      "---",
      "PREMISE 1 [CONDITIONAL]: If [P] then [Q]",
      "PREMISE 2 [AFFIRMATION]: [P] is true",
      "INFERENCE: MODUS_PONENS",
      "CONCLUSION: Therefore, [Q] is true",
      "VALIDITY: VALID",
      "EXPLANATION: [Why this follows]",
      "---",
      "",
      "If modus ponens cannot be applied, respond with:",
      "INFERENCE: NO_VALID_INFERENCE",
      "REASON: [Explain why modus ponens doesn't apply]"
    ]

-- | Prompt specifically for modus tollens reasoning
modusTollensPrompt :: Text
modusTollensPrompt =
  T.unlines
    [ "You are a formal logic assistant. Apply MODUS TOLLENS to the given observations.",
      "",
      "MODUS TOLLENS (Denying the Consequent):",
      "  Premise 1: If P then Q  (P → Q)",
      "  Premise 2: Q is false   (¬Q)",
      "  Conclusion: P is false  (¬P)",
      "",
      "Your task:",
      "1. Identify a conditional statement (If P then Q) from the observations",
      "2. Identify a negation of the consequent (Q is false)",
      "3. Derive the conclusion (P is false)",
      "",
      "RESPOND IN THIS EXACT FORMAT:",
      "---",
      "PREMISE 1 [CONDITIONAL]: If [P] then [Q]",
      "PREMISE 2 [NEGATION]: [Q] is false",
      "INFERENCE: MODUS_TOLLENS",
      "CONCLUSION: Therefore, [P] is false",
      "VALIDITY: VALID",
      "EXPLANATION: [Why this follows]",
      "---",
      "",
      "If modus tollens cannot be applied, respond with:",
      "INFERENCE: NO_VALID_INFERENCE",
      "REASON: [Explain why modus tollens doesn't apply]"
    ]

-- ============================================================================
-- Reasoning Workflows
-- ============================================================================

-- | Main reasoning workflow - automatically selects appropriate inference rule
--   Given observations, constructs a logically valid argument
reasoningWorkflow :: Workflow Text
reasoningWorkflow = do
  observations <- getUserPrompt

  let prompt =
        T.unlines
          [ formalReasoningPrompt,
            "",
            "=== OBSERVATIONS ===",
            observations,
            "",
            "Analyze these observations and construct a valid logical argument."
          ]

  llmCall prompt

-- | Workflow specifically for modus ponens reasoning
--   Attempts to find and apply P → Q, P ⊢ Q
modusPonensWorkflow :: Workflow Text
modusPonensWorkflow = do
  observations <- getUserPrompt

  let prompt =
        T.unlines
          [ modusPonensPrompt,
            "",
            "=== OBSERVATIONS ===",
            observations,
            "",
            "Apply modus ponens to derive a conclusion."
          ]

  llmCall prompt

-- | Workflow specifically for modus tollens reasoning
--   Attempts to find and apply P → Q, ¬Q ⊢ ¬P
modusTollensWorkflow :: Workflow Text
modusTollensWorkflow = do
  observations <- getUserPrompt

  let prompt =
        T.unlines
          [ modusTollensPrompt,
            "",
            "=== OBSERVATIONS ===",
            observations,
            "",
            "Apply modus tollens to derive a conclusion."
          ]

  llmCall prompt

-- ============================================================================
-- Parsing
-- ============================================================================

-- | Parse the LLM's structured response into a ReasoningResult
--   Returns Nothing if parsing fails
parseReasoningResult :: Text -> Text -> Maybe ReasoningResult
parseReasoningResult observations response =
  let lines' = T.lines response

      -- Extract sections
      findLine prefix =
        T.strip . T.drop (T.length prefix)
          <$> filter (T.isPrefixOf prefix) lines'

      premises = extractPremises lines'
      inferenceRule = parseInferenceRule $ head' $ findLine "INFERENCE:"
      conclusion = head' $ findLine "CONCLUSION:"
      validity = parseValidity $ head' $ findLine "VALIDITY:"
      explanation = head' $ findLine "EXPLANATION:"
      soundness = parseSoundness $ head' $ findLine "SOUNDNESS:"
   in case (inferenceRule, conclusion, validity) of
        (Just rule, Just conc, Just val) ->
          Just $
            ReasoningResult
              { resObservations = observations,
                resArgument =
                  Argument
                    { argPremises = premises,
                      argRule = rule,
                      argConclusion = Conclusion conc val
                    },
                resExplanation = maybe "" id explanation,
                resIsSound = soundness == Just True,
                resIsValid = val == Valid
              }
        _ -> Nothing
  where
    head' [] = Nothing
    head' (x : _) = Just x

-- | Extract premises from response lines
extractPremises :: [Text] -> [Premise]
extractPremises lines' =
  let premiseLines = filter isPremiseLine lines'
   in zipWith parsePremise [1 ..] premiseLines
  where
    isPremiseLine l =
      T.isPrefixOf "P1" l
        || T.isPrefixOf "P2" l
        || T.isPrefixOf "PREMISE" l

-- | Parse a single premise line
parsePremise :: Int -> Text -> Premise
parsePremise n line =
  Premise
    { premiseId = n,
      premiseText = extractPremiseText line,
      premiseType = extractPremiseType line
    }

extractPremiseText :: Text -> Text
extractPremiseText line =
  let afterColon = T.drop 1 $ T.dropWhile (/= ':') line
   in T.strip afterColon

extractPremiseType :: Text -> PremiseType
extractPremiseType line
  | T.isInfixOf "CONDITIONAL" line = Conditional
  | T.isInfixOf "AFFIRMATION" line = Affirmation
  | T.isInfixOf "NEGATION" line = Negation
  | otherwise = Observation

-- | Parse inference rule from text
parseInferenceRule :: Maybe Text -> Maybe InferenceRule
parseInferenceRule Nothing = Nothing
parseInferenceRule (Just t)
  | T.isInfixOf "MODUS_PONENS" t || T.isInfixOf "MODUS PONENS" t = Just ModusPonens
  | T.isInfixOf "MODUS_TOLLENS" t || T.isInfixOf "MODUS TOLLENS" t = Just ModusTollens
  | T.isInfixOf "NO_VALID" t || T.isInfixOf "NO VALID" t = Just NoInference
  | otherwise = Nothing

-- | Parse validity from text
parseValidity :: Maybe Text -> Maybe Validity
parseValidity Nothing = Nothing
parseValidity (Just t)
  | T.isInfixOf "VALID" t && not (T.isInfixOf "INVALID" t) = Just Valid
  | T.isInfixOf "INVALID" t = Just Invalid
  | otherwise = Just Uncertain

-- | Parse soundness from text
parseSoundness :: Maybe Text -> Maybe Bool
parseSoundness Nothing = Nothing
parseSoundness (Just t)
  | T.isInfixOf "SOUND" t && not (T.isInfixOf "UNSOUND" t) = Just True
  | T.isInfixOf "UNSOUND" t = Just False
  | otherwise = Nothing
