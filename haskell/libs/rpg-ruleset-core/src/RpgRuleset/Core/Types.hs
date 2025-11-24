{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module RpgRuleset.Core.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Set (Set)
import Data.Map.Strict (Map)

-- | Rule ID with format validation: ^[A-Z]{2,6}-\d{3}$
-- Examples: CHAR-001, CMBT-042, CHRGEN-100
newtype RuleId = RuleId { unRuleId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Category for organizing rules
newtype Category = Category { unCategory :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Tag for rule classification (lowercase)
newtype Tag = Tag { unTag :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | System identifier
newtype SystemId = SystemId { unSystemId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | World identifier
newtype WorldId = WorldId { unWorldId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Markdown content
type MarkdownContent = Text

-- | Formula name
type FormulaName = Text

-- | Visibility control for access management
data Visibility
  = Public    -- ^ Visible to all players
  | GMOnly    -- ^ Only visible to Game Master
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON Visibility
instance ToJSON Visibility

-- | Version following semantic versioning
data Version = Version
  { vMajor :: !Int
  , vMinor :: !Int
  , vPatch :: !Int
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON Version
instance ToJSON Version

-- | Changelog entry for version history
data ChangelogEntry = ChangelogEntry
  { ceVersion :: !Version
  , ceDate :: !UTCTime
  , ceDescription :: !Text
  , ceAuthor :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON ChangelogEntry
instance ToJSON ChangelogEntry

-- | System type for inheritance
data SystemType
  = BaseSystem    -- ^ Standalone system
  | VariantSystem -- ^ Extends a base system
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON SystemType
instance ToJSON SystemType

-- | Cross-system reference
data CrossSystemRef = CrossSystemRef
  { csrSystemId :: !SystemId
  , csrRuleId :: !RuleId
  } deriving (Show, Eq, Generic)

instance FromJSON CrossSystemRef
instance ToJSON CrossSystemRef

-- | User role for access control
data UserRole
  = Player
  | GameMaster
  deriving (Show, Eq, Enum, Bounded, Generic)

instance FromJSON UserRole
instance ToJSON UserRole

-- | Formula AST (placeholder for Phase 2c)
data Formula = Formula
  { formulaExpression :: !Text
  , formulaDescription :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON Formula
instance ToJSON Formula

-- | Condition AST (placeholder for Phase 2c)
data Condition = Condition
  { conditionExpression :: !Text
  , conditionDescription :: !(Maybe Text)
  } deriving (Show, Eq, Generic)

instance FromJSON Condition
instance ToJSON Condition

-- | Core categories as defined in spec
data CoreCategory
  = CharacterCreation
  | WorldBuilding
  | Interactions
  deriving (Show, Eq, Enum, Bounded)

-- | Convert core category to text representation
coreCategoryToText :: CoreCategory -> Text
coreCategoryToText CharacterCreation = "character-creation"
coreCategoryToText WorldBuilding = "world-building"
coreCategoryToText Interactions = "interactions"

-- | Parse text to core category
parseCategory :: Text -> Maybe CoreCategory
parseCategory "character-creation" = Just CharacterCreation
parseCategory "world-building" = Just WorldBuilding
parseCategory "interactions" = Just Interactions
parseCategory _ = Nothing

-- | Check if a RuleId matches the required format
validateRuleIdFormat :: RuleId -> Bool
validateRuleIdFormat (RuleId rid) =
  case T.splitOn "-" rid of
    [prefix, num] ->
      let prefixLen = T.length prefix
          isUpperAlpha = T.all (`elem` ['A'..'Z']) prefix
          isThreeDigits = T.length num == 3 && T.all (`elem` ['0'..'9']) num
      in prefixLen >= 2 && prefixLen <= 6 && isUpperAlpha && isThreeDigits
    _ -> False

-- | Common rule ID prefixes (suggested but not enforced)
suggestedPrefixes :: [Text]
suggestedPrefixes = ["CHAR", "WRLD", "INTR", "CMBT", "SOCL", "GEOG", "CLASS"]