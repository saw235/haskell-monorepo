{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Core.Rule where

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import RpgRuleset.Core.Types

-- | The fundamental unit representing a single game rule or mechanic
data Rule = Rule
  { -- Identity (FR-002, FR-003)
    ruleId :: !RuleId
  , ruleCategory :: !Category
  , ruleSystemId :: !SystemId

  -- Content (FR-001)
  , ruleTitle :: !(Maybe Text)
  , ruleContent :: !MarkdownContent

  -- Metadata (FR-004, FR-005)
  , ruleTags :: !(Set Tag)
  , ruleVisibility :: !Visibility

  -- Versioning (FR-029)
  , ruleVersion :: !Version
  , ruleChangelog :: ![ChangelogEntry]

  -- Relationships (FR-006)
  , ruleRelatedRules :: ![RuleId]
  , ruleCrossSystemRefs :: ![CrossSystemRef]

  -- Advanced Features (Phase 2c)
  , ruleConditions :: !(Maybe [Condition])
  , ruleFormulas :: !(Maybe (Map FormulaName Formula))

  -- File metadata
  , ruleSourceFile :: !FilePath
  , ruleLoadedAt :: !(Maybe UTCTime)
  } deriving (Show, Eq, Generic)

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \o -> do
    ruleId <- o .: "rule_id"
    ruleCategory <- o .: "category"
    ruleSystemId <- o .:? "system_id" .!= SystemId "default"
    ruleTitle <- o .:? "title"
    ruleContent <- o .:? "content" .!= ""
    ruleTags <- o .:? "tags" .!= Set.empty
    ruleVisibility <- o .:? "visibility" .!= Public
    ruleVersion <- o .:? "version" .!= Version 1 0 0
    ruleChangelog <- o .:? "changelog" .!= []
    ruleRelatedRules <- o .:? "related_rules" .!= []
    ruleCrossSystemRefs <- o .:? "cross_system_refs" .!= []
    ruleConditions <- o .:? "conditions"
    ruleFormulas <- o .:? "formulas"
    let ruleSourceFile = ""
        ruleLoadedAt = Nothing
    return Rule{..}

instance ToJSON Rule where
  toJSON Rule{..} = object
    [ "rule_id" .= ruleId
    , "category" .= ruleCategory
    , "system_id" .= ruleSystemId
    , "title" .= ruleTitle
    , "content" .= ruleContent
    , "tags" .= ruleTags
    , "visibility" .= ruleVisibility
    , "version" .= ruleVersion
    , "changelog" .= ruleChangelog
    , "related_rules" .= ruleRelatedRules
    , "cross_system_refs" .= ruleCrossSystemRefs
    , "conditions" .= ruleConditions
    , "formulas" .= ruleFormulas
    , "source_file" .= ruleSourceFile
    ]

-- | Create a minimal rule with required fields
mkRule :: RuleId -> Category -> SystemId -> MarkdownContent -> Rule
mkRule rid cat sid content = Rule
  { ruleId = rid
  , ruleCategory = cat
  , ruleSystemId = sid
  , ruleTitle = Nothing
  , ruleContent = content
  , ruleTags = Set.empty
  , ruleVisibility = Public
  , ruleVersion = Version 1 0 0
  , ruleChangelog = []
  , ruleRelatedRules = []
  , ruleCrossSystemRefs = []
  , ruleConditions = Nothing
  , ruleFormulas = Nothing
  , ruleSourceFile = ""
  , ruleLoadedAt = Nothing
  }

-- | Check if a rule is visible to a given user role
isVisibleTo :: UserRole -> Rule -> Bool
isVisibleTo GameMaster _ = True
isVisibleTo Player rule = ruleVisibility rule == Public

-- | Get all tags as a list of text
getRuleTags :: Rule -> [Text]
getRuleTags = map unTag . Set.toList . ruleTags