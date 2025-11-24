{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Core.System where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import RpgRuleset.Core.Types
import RpgRuleset.Core.Rule (Rule, ruleId)

-- | Represents a complete RPG ruleset (base or variant)
data System = System
  { -- Identity
    systemId :: !SystemId
  , systemName :: !Text
  , systemDescription :: !(Maybe Text)
  , systemType :: !SystemType

  -- Inheritance (Phase 2b)
  , systemBaseSystem :: !(Maybe SystemId)

  -- Content
  , systemRules :: !(Map RuleId Rule)
  , systemCategories :: ![Category]

  -- Metadata
  , systemVersion :: !Version
  , systemAuthors :: ![Text]
  , systemLicense :: !(Maybe Text)

  -- File metadata
  , systemRootPath :: !FilePath
  } deriving (Show, Eq, Generic)

instance FromJSON System where
  parseJSON = withObject "System" $ \o -> do
    systemId <- o .: "system_id"
    systemName <- o .: "name"
    systemDescription <- o .:? "description"
    systemType <- o .:? "type" .!= BaseSystem
    systemBaseSystem <- o .:? "base_system"
    let systemRules = Map.empty  -- Rules loaded separately
    systemCategories <- o .:? "categories" .!= defaultCategories
    systemVersion <- o .:? "version" .!= Version 1 0 0
    systemAuthors <- o .:? "authors" .!= []
    systemLicense <- o .:? "license"
    let systemRootPath = ""
    return System{..}

instance ToJSON System where
  toJSON System{..} = object
    [ "system_id" .= systemId
    , "name" .= systemName
    , "description" .= systemDescription
    , "type" .= systemType
    , "base_system" .= systemBaseSystem
    , "categories" .= systemCategories
    , "version" .= systemVersion
    , "authors" .= systemAuthors
    , "license" .= systemLicense
    ]

-- | Default categories for a new system
defaultCategories :: [Category]
defaultCategories =
  [ Category "character-creation"
  , Category "world-building"
  , Category "interactions"
  ]

-- | Create a minimal base system
mkBaseSystem :: SystemId -> Text -> System
mkBaseSystem sid name = System
  { systemId = sid
  , systemName = name
  , systemDescription = Nothing
  , systemType = BaseSystem
  , systemBaseSystem = Nothing
  , systemRules = Map.empty
  , systemCategories = defaultCategories
  , systemVersion = Version 1 0 0
  , systemAuthors = []
  , systemLicense = Nothing
  , systemRootPath = ""
  }

-- | Create a variant system that extends a base
mkVariantSystem :: SystemId -> Text -> SystemId -> System
mkVariantSystem sid name baseId = (mkBaseSystem sid name)
  { systemType = VariantSystem
  , systemBaseSystem = Just baseId
  }

-- | Check if system is a variant
isVariant :: System -> Bool
isVariant sys = systemType sys == VariantSystem

-- | Get the base system ID for a variant (Nothing for base systems)
getBaseSystemId :: System -> Maybe SystemId
getBaseSystemId = systemBaseSystem

-- | Add a rule to a system
addRule :: Rule -> System -> System
addRule rule sys = sys
  { systemRules = Map.insert (ruleId rule) rule (systemRules sys)
  }

-- | Get all rules in a system
getAllRules :: System -> [Rule]
getAllRules = Map.elems . systemRules

-- | Look up a rule by ID
lookupRule :: RuleId -> System -> Maybe Rule
lookupRule rid = Map.lookup rid . systemRules