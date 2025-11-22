{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Capabilities
-- Description : Capability application logic
-- Copyright   : (c) 2025
-- License     : MIT
module AgenticFramework.Workflow.Capabilities
  ( -- * Capability Application
    applyCapability,
    applyCapabilities,

    -- * Capability Lookup
    findCapability,
    findCapabilityMaybe,
    lookupCapability,

    -- * Capability Loading
    capabilityFromDef,

    -- * Runtime Capability Composition (User Story 5 - FR-015, FR-007)
    composeCapabilities,
    mergeCapabilities,
    stackCapabilities,
    CapabilityComposition (..),
  )
where

import AgenticFramework.Workflow.Types (Capability (..), CapabilityDef (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | JSON instance for CapabilityDef
instance FromJSON CapabilityDef where
  parseJSON = withObject "CapabilityDef" $ \v ->
    CapabilityDef
      <$> v .: "name"
      <*> v .: "description"
      <*> v .:? "parameters"
      <*> v .:? "modifierType"
      <*> v .:? "modifierValue"

instance ToJSON CapabilityDef where
  toJSON def =
    Aeson.object
      [ "name" Aeson..= capDefName def,
        "description" Aeson..= capDefDescription def,
        "parameters" Aeson..= capDefParameters def,
        "modifierType" Aeson..= capDefModifierType def,
        "modifierValue" Aeson..= capDefModifierValue def
      ]

-- | Apply a single capability to a prompt
applyCapability :: Capability -> Text -> Text
applyCapability cap prompt = (capModifier cap) prompt

-- | Apply a list of capabilities to a prompt
--   Capabilities are applied in order: c1, then c2, etc.
--   Result = cn(...c2(c1(prompt))...)
applyCapabilities :: [Capability] -> Text -> Text
applyCapabilities caps prompt = foldl (flip applyCapability) prompt caps

-- ============================================================================
-- Capability Lookup Functions
-- ============================================================================

-- | Find a capability by name, returning a default identity capability if not found
--   This is safe to use in workflows - never throws an exception
findCapability :: Text -> [Capability] -> Capability
findCapability name caps =
  case findCapabilityMaybe name caps of
    Just cap -> cap
    Nothing -> defaultCapability name

-- | Find a capability by name, returning Nothing if not found
findCapabilityMaybe :: Text -> [Capability] -> Maybe Capability
findCapabilityMaybe name caps = find (\c -> capName c == name) caps

-- | Alias for findCapabilityMaybe for consistency with standard Haskell naming
lookupCapability :: Text -> [Capability] -> Maybe Capability
lookupCapability = findCapabilityMaybe

-- | Default capability used when a capability is not found
--   Has identity modifier (does not modify the prompt)
defaultCapability :: Text -> Capability
defaultCapability name =
  Capability
    { capName = name,
      capDescription = "Default " <> name <> " capability (not found)",
      capModifier = id,
      capParameters = Nothing
    }

-- | Convert a CapabilityDef (loaded from JSON) to a Capability (runtime)
--   The modifier is constructed based on modifierType and modifierValue
capabilityFromDef :: CapabilityDef -> Capability
capabilityFromDef def =
  Capability
    { capName = capDefName def,
      capDescription = capDefDescription def,
      capModifier = buildModifier (capDefModifierType def) (capDefModifierValue def),
      capParameters = capDefParameters def
    }

-- | Build a prompt modifier function based on type and value
buildModifier :: Maybe Text -> Maybe Text -> (Text -> Text)
buildModifier (Just "prefix") (Just val) = \prompt -> val <> prompt
buildModifier (Just "suffix") (Just val) = \prompt -> prompt <> val
buildModifier (Just "wrap") (Just val) = \prompt -> val <> prompt <> val
buildModifier (Just "replace") (Just val) = const val
buildModifier _ _ = id -- Default: identity function (no modification)

-- ============================================================================
-- Runtime Capability Composition (User Story 5 - FR-015, FR-007)
-- ============================================================================

-- | Composition strategy for combining capabilities
data CapabilityComposition
  = Sequential -- Apply capabilities one after another
  | Parallel -- Apply all capabilities to original prompt and combine results
  | Override -- Later capability replaces earlier
  deriving (Show, Eq)

-- | Compose two capabilities into one using sequential composition
--   The resulting capability applies the first, then the second modifier
composeCapabilities :: Capability -> Capability -> Capability
composeCapabilities cap1 cap2 =
  Capability
    { capName = capName cap1 <> "+" <> capName cap2,
      capDescription = "Composed: " <> capDescription cap1 <> " then " <> capDescription cap2,
      capModifier = capModifier cap2 . capModifier cap1,
      capParameters = mergeParams (capParameters cap1) (capParameters cap2)
    }

-- | Merge parameters from two capabilities (second takes precedence)
mergeParams :: Maybe Aeson.Value -> Maybe Aeson.Value -> Maybe Aeson.Value
mergeParams Nothing p2 = p2
mergeParams p1 Nothing = p1
mergeParams (Just (Aeson.Object o1)) (Just (Aeson.Object o2)) =
  Just $ Aeson.Object (o1 <> o2)
mergeParams _ p2 = p2

-- | Merge a list of capabilities into a single capability
--   Uses sequential composition by default
mergeCapabilities :: [Capability] -> Maybe Capability
mergeCapabilities [] = Nothing
mergeCapabilities [c] = Just c
mergeCapabilities (c : cs) = Just $ foldl composeCapabilities c cs

-- | Stack capabilities for step-level application
--   Returns a new capability that applies all stacked capabilities
stackCapabilities :: [Capability] -> [Capability] -> [Capability]
stackCapabilities base stepLevel = base ++ stepLevel
