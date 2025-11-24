{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Query.Engine
  ( -- * Query Types
    Query (..),
    QueryResult (..),
    ScoredRule (..),

    -- * Query Execution
    executeQuery,
    applyFilters,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import RpgRuleset.Core.Rule
import RpgRuleset.Core.Types
import RpgRuleset.Query.Index
import RpgRuleset.Query.Ranking (ScoredRule (..), scoreRule)

-- | Query specification
data Query = Query
  { qKeywords :: ![Text],
    qFilterCategory :: !(Maybe Category),
    qFilterSystem :: !(Maybe SystemId),
    qFilterTags :: ![Tag],
    qFilterVisibility :: !(Maybe Visibility),
    qLimit :: !Int,
    qOffset :: !Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Query

instance ToJSON Query

-- | Query result with pagination info
data QueryResult = QueryResult
  { qrResults :: ![ScoredRule],
    qrTotalCount :: !Int,
    qrLimit :: !Int,
    qrOffset :: !Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON QueryResult

instance FromJSON QueryResult

-- | Execute a query against the index
executeQuery :: Query -> QueryIndex -> QueryResult
executeQuery query idx =
  let -- Get all rules
      allRules = getAllRules idx

      -- Apply filters
      filtered = applyFilters query allRules

      -- Score and sort
      scored = map (scoreRule (qKeywords query)) filtered
      sorted = sortBy (comparing (Down . srScore)) scored

      -- Apply pagination
      totalCount = length sorted
      paginated = take (qLimit query) $ drop (qOffset query) sorted
   in QueryResult
        { qrResults = paginated,
          qrTotalCount = totalCount,
          qrLimit = qLimit query,
          qrOffset = qOffset query
        }

-- | Apply all filters to a list of rules
applyFilters :: Query -> [Rule] -> [Rule]
applyFilters Query {..} rules =
  let f1 = maybe id filterByCategory qFilterCategory
      f2 = maybe id filterBySystem qFilterSystem
      f3 = if null qFilterTags then id else filterByTags qFilterTags
      f4 = maybe id filterByVisibility qFilterVisibility
   in f4 $ f3 $ f2 $ f1 rules

-- | Filter rules by category
filterByCategory :: Category -> [Rule] -> [Rule]
filterByCategory cat = filter ((== cat) . ruleCategory)

-- | Filter rules by system
filterBySystem :: SystemId -> [Rule] -> [Rule]
filterBySystem sid = filter ((== sid) . ruleSystemId)

-- | Filter rules by tags (rule must have ALL specified tags)
filterByTags :: [Tag] -> [Rule] -> [Rule]
filterByTags tags = filter hasAllTags
  where
    tagSet = Set.fromList tags
    hasAllTags rule = tagSet `Set.isSubsetOf` ruleTags rule

-- | Filter rules by visibility
filterByVisibility :: Visibility -> [Rule] -> [Rule]
filterByVisibility GMOnly = id -- GM sees everything
filterByVisibility Public = filter ((== Public) . ruleVisibility)
