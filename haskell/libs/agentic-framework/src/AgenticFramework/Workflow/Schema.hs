{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Schema
-- Description : JSON schema definitions for capability files
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Defines the JSON schema for capability definition files.
-- Capabilities can be loaded from JSON files with the following structure:
--
-- @
-- {
--   "name": "reasoning",
--   "description": "Break down complex problems step-by-step",
--   "modifierType": "prefix",
--   "modifierValue": "Think step by step: ",
--   "parameters": {
--     "style": "chain-of-thought",
--     "maxSteps": 10
--   }
-- }
-- @
module AgenticFramework.Workflow.Schema
  ( -- * Schema Types
    CapabilitySchema (..),
    CapabilitySetSchema (..),
    ModifierType (..),

    -- * Schema Validation
    validateCapabilitySchema,
    schemaToCapabilityDef,
  )
where

import AgenticFramework.Workflow.Types (CapabilityDef (..))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Supported modifier types for capabilities
data ModifierType
  = PrefixModifier -- Prepends text to prompt
  | SuffixModifier -- Appends text to prompt
  | WrapModifier -- Wraps prompt with text on both sides
  | ReplaceModifier -- Replaces prompt entirely
  | IdentityModifier -- No modification (default)
  deriving (Show, Eq, Generic)

instance FromJSON ModifierType where
  parseJSON = withText "ModifierType" $ \t ->
    case T.toLower t of
      "prefix" -> pure PrefixModifier
      "suffix" -> pure SuffixModifier
      "wrap" -> pure WrapModifier
      "replace" -> pure ReplaceModifier
      "identity" -> pure IdentityModifier
      _ -> fail $ "Unknown modifier type: " <> T.unpack t

instance ToJSON ModifierType where
  toJSON PrefixModifier = Aeson.String "prefix"
  toJSON SuffixModifier = Aeson.String "suffix"
  toJSON WrapModifier = Aeson.String "wrap"
  toJSON ReplaceModifier = Aeson.String "replace"
  toJSON IdentityModifier = Aeson.String "identity"

-- | Schema for a single capability definition in JSON
data CapabilitySchema = CapabilitySchema
  { schemaName :: Text,
    schemaDescription :: Text,
    schemaModifierType :: Maybe ModifierType,
    schemaModifierValue :: Maybe Text,
    schemaParameters :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON CapabilitySchema where
  parseJSON = withObject "CapabilitySchema" $ \v ->
    CapabilitySchema
      <$> v .: "name"
      <*> v .: "description"
      <*> v .:? "modifierType"
      <*> v .:? "modifierValue"
      <*> v .:? "parameters"

instance ToJSON CapabilitySchema where
  toJSON schema =
    Aeson.object
      [ "name" Aeson..= schemaName schema,
        "description" Aeson..= schemaDescription schema,
        "modifierType" Aeson..= schemaModifierType schema,
        "modifierValue" Aeson..= schemaModifierValue schema,
        "parameters" Aeson..= schemaParameters schema
      ]

-- | Schema for a set of capabilities (for loading multiple from one file)
data CapabilitySetSchema = CapabilitySetSchema
  { setName :: Text,
    setDescription :: Maybe Text,
    setCapabilities :: [CapabilitySchema]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CapabilitySetSchema where
  parseJSON = withObject "CapabilitySetSchema" $ \v ->
    CapabilitySetSchema
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .: "capabilities"

instance ToJSON CapabilitySetSchema where
  toJSON set =
    Aeson.object
      [ "name" Aeson..= setName set,
        "description" Aeson..= setDescription set,
        "capabilities" Aeson..= setCapabilities set
      ]

-- | Validation errors for capability schemas
data SchemaValidationError
  = EmptyName
  | EmptyDescription
  | InvalidModifierConfig Text
  deriving (Show, Eq)

-- | Validate a capability schema
validateCapabilitySchema :: CapabilitySchema -> Either SchemaValidationError ()
validateCapabilitySchema schema
  | T.null (schemaName schema) = Left EmptyName
  | T.null (schemaDescription schema) = Left EmptyDescription
  | hasModifierType && not hasModifierValue =
      Left $ InvalidModifierConfig "modifierType requires modifierValue"
  | otherwise = Right ()
  where
    hasModifierType = case schemaModifierType schema of
      Just IdentityModifier -> False
      Just _ -> True
      Nothing -> False
    hasModifierValue = case schemaModifierValue schema of
      Just v -> not (T.null v)
      Nothing -> False

-- | Convert a validated schema to a CapabilityDef
schemaToCapabilityDef :: CapabilitySchema -> CapabilityDef
schemaToCapabilityDef schema =
  CapabilityDef
    { capDefName = schemaName schema,
      capDefDescription = schemaDescription schema,
      capDefParameters = schemaParameters schema,
      capDefModifierType = modifierTypeToText <$> schemaModifierType schema,
      capDefModifierValue = schemaModifierValue schema
    }

-- | Convert ModifierType to its text representation
modifierTypeToText :: ModifierType -> Text
modifierTypeToText PrefixModifier = "prefix"
modifierTypeToText SuffixModifier = "suffix"
modifierTypeToText WrapModifier = "wrap"
modifierTypeToText ReplaceModifier = "replace"
modifierTypeToText IdentityModifier = "identity"
