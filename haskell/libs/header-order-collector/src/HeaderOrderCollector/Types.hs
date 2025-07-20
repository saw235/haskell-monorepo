{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HeaderOrderCollector.Types
  ( -- * Core Types
    HeaderOrder(..)
  , HeaderPattern(..)
  , BrowserProfile(..)
  , HeaderDatabase(..)
  
    -- * Header Information
  , HeaderName
  , HeaderValue
  , HeaderPosition
  , HeaderFrequency
  
    -- * Browser Classification
  , BrowserType(..)
  , BrowserVersion(..)
  , DeviceType(..)
  , OperatingSystem(..)
  
    -- * Pattern Matching
  , PatternRule(..)
  , MatchingStrategy(..)
  , OrderingRule(..)
  
    -- * Statistics
  , HeaderStatistics(..)
  , OrderFrequency(..)
  , PatternDistribution(..)
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson (ToJSONKey, FromJSONKey)
import GHC.Generics (Generic)

-- | Type aliases for clarity
type HeaderName = Text
type HeaderValue = Text
type HeaderPosition = Int
type HeaderFrequency = Double

-- | Ordered list of headers for a specific browser/context
data HeaderOrder = HeaderOrder
  { orderHeaders :: [HeaderName]
  , orderBrowser :: BrowserType
  , orderVersion :: BrowserVersion
  , orderDevice :: DeviceType
  , orderOS :: OperatingSystem
  , orderContext :: Text  -- e.g., "navigation", "xhr", "fetch"
  , orderFrequency :: HeaderFrequency
  } deriving (Show, Eq, Generic)

instance ToJSON HeaderOrder
instance FromJSON HeaderOrder

-- | Pattern of header ordering for pattern matching
data HeaderPattern = HeaderPattern
  { patternId :: Text
  , patternName :: Text
  , patternHeaders :: [HeaderName]
  , patternRules :: [PatternRule]
  , patternScore :: Double
  , patternMetadata :: Map Text Text
  } deriving (Show, Eq, Generic)

instance ToJSON HeaderPattern
instance FromJSON HeaderPattern

-- | Browser profile containing header ordering information
data BrowserProfile = BrowserProfile
  { profileBrowser :: BrowserType
  , profileVersionRange :: (BrowserVersion, BrowserVersion)
  , profileDevices :: [DeviceType]
  , profileOperatingSystems :: [OperatingSystem]
  , profileHeaderOrders :: [HeaderOrder]
  , profilePatterns :: [HeaderPattern]
  , profileLastUpdated :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON BrowserProfile
instance FromJSON BrowserProfile

-- | Database of header ordering patterns
data HeaderDatabase = HeaderDatabase
  { databaseProfiles :: Map BrowserType BrowserProfile
  , databasePatterns :: Map Text HeaderPattern
  , databaseStatistics :: HeaderStatistics
  , databaseVersion :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON HeaderDatabase
instance FromJSON HeaderDatabase

-- | Browser types
data BrowserType
  = Chrome
  | Firefox
  | Safari
  | Edge
  | Opera
  | InternetExplorer
  | UnknownBrowser Text
  deriving (Show, Eq, Ord, Generic)

instance ToJSON BrowserType
instance FromJSON BrowserType
instance ToJSONKey BrowserType
instance FromJSONKey BrowserType

-- | Browser version information
data BrowserVersion = BrowserVersion
  { versionMajor :: Int
  , versionMinor :: Int
  , versionPatch :: Int
  , versionBuild :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON BrowserVersion
instance FromJSON BrowserVersion

-- | Device types
data DeviceType
  = Desktop
  | Mobile
  | Tablet
  | SmartTV
  | Wearable
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON DeviceType
instance FromJSON DeviceType
instance ToJSONKey DeviceType
instance FromJSONKey DeviceType

-- | Operating systems
data OperatingSystem
  = Windows
  | MacOS
  | Linux
  | Android
  | IOS
  | ChromeOS
  | UnknownOS Text
  deriving (Show, Eq, Generic)

instance ToJSON OperatingSystem
instance FromJSON OperatingSystem

-- | Rules for pattern matching
data PatternRule
  = MustHaveHeader HeaderName
  | MustNotHaveHeader HeaderName
  | HeaderMustBeAfter HeaderName HeaderName
  | HeaderMustBeBefore HeaderName HeaderName
  | HeaderMustBeAtPosition HeaderName HeaderPosition
  | ConsecutiveHeaders [HeaderName]
  | ConditionalRule Text PatternRule  -- Condition -> Rule
  deriving (Show, Eq, Generic)

instance ToJSON PatternRule
instance FromJSON PatternRule

-- | Strategy for matching headers to patterns
data MatchingStrategy
  = ExactMatch        -- Exact header order match
  | SubsetMatch       -- Headers must be subset in correct order
  | FuzzyMatch Double -- Fuzzy matching with similarity threshold
  | WeightedMatch     -- Weighted scoring based on header importance
  deriving (Show, Eq, Generic)

instance ToJSON MatchingStrategy
instance FromJSON MatchingStrategy

-- | Rules for ordering headers
data OrderingRule
  = AlphabeticalOrder
  | FrequencyOrder      -- Most common headers first
  | BrowserSpecificOrder BrowserType
  | CustomOrder [HeaderName]
  | PriorityOrder (Map HeaderName Int)  -- Header -> Priority
  deriving (Show, Eq, Generic)

instance ToJSON OrderingRule
instance FromJSON OrderingRule

-- | Statistics about header usage and ordering
data HeaderStatistics = HeaderStatistics
  { statsHeaderFrequency :: Map HeaderName HeaderFrequency
  , statsOrderFrequency :: Map [HeaderName] OrderFrequency
  , statsBrowserDistribution :: Map BrowserType Double
  , statsDeviceDistribution :: Map DeviceType Double
  , statsCommonPatterns :: [PatternDistribution]
  , statsTotalSamples :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON HeaderStatistics
instance FromJSON HeaderStatistics

-- | Frequency of specific header orders
data OrderFrequency = OrderFrequency
  { frequencyOrder :: [HeaderName]
  , frequencyCount :: Int
  , frequencyPercentage :: Double
  , frequencyBrowsers :: Map BrowserType Int
  } deriving (Show, Eq, Generic)

instance ToJSON OrderFrequency
instance FromJSON OrderFrequency

-- | Distribution of pattern usage
data PatternDistribution = PatternDistribution
  { distributionPattern :: Text
  , distributionFrequency :: Double
  , distributionBrowsers :: [BrowserType]
  , distributionConfidence :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON PatternDistribution
instance FromJSON PatternDistribution

-- | Comparison operations for browser versions
instance Ord BrowserVersion where
  compare (BrowserVersion maj1 min1 pat1 _) (BrowserVersion maj2 min2 pat2 _) =
    case compare maj1 maj2 of
      EQ -> case compare min1 min2 of
        EQ -> compare pat1 pat2
        result -> result
      result -> result 