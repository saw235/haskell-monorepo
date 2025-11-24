{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Query.Ranking
  ( scoreRule,
    ScoredRule (..),
    rankingWeights,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import RpgRuleset.Core.Rule
import RpgRuleset.Core.Types

-- | A rule with its relevance score
data ScoredRule = ScoredRule
  { srRule :: !Rule,
    srScore :: !Double
  }
  deriving (Show, Eq, Generic)

instance ToJSON ScoredRule

instance FromJSON ScoredRule

-- | Ranking weights for different match types
data RankingWeights = RankingWeights
  { -- | Weight for title matches
    wTitle :: !Double,
    -- | Weight for content matches
    wContent :: !Double,
    -- | Weight for tag matches
    wTag :: !Double,
    -- | Weight for category matches
    wCategory :: !Double
  }
  deriving (Show, Eq)

-- | Default ranking weights per spec
rankingWeights :: RankingWeights
rankingWeights =
  RankingWeights
    { wTitle = 5.0,
      wContent = 1.0,
      wTag = 2.0,
      wCategory = 10.0
    }

-- | Score a rule based on keyword matches
scoreRule :: [Text] -> Rule -> ScoredRule
scoreRule keywords rule =
  let score = sum $ map (scoreKeyword rule) keywords
   in ScoredRule
        { srRule = rule,
          srScore = score
        }

-- | Score a single keyword against a rule
scoreKeyword :: Rule -> Text -> Double
scoreKeyword Rule {..} keyword =
  let kw = T.toLower keyword
      RankingWeights {..} = rankingWeights

      -- Title score
      titleScore = case ruleTitle of
        Nothing -> 0.0
        Just title ->
          if kw `T.isInfixOf` T.toLower title
            then wTitle
            else 0.0

      -- Content score
      contentScore =
        let contentLower = T.toLower ruleContent
            occurrences = countOccurrences kw contentLower
         in wContent * fromIntegral occurrences

      -- Tag score
      tagScore =
        let matchingTags = filter (keywordMatchesTag kw) (Set.toList ruleTags)
         in wTag * fromIntegral (length matchingTags)

      -- Category score
      categoryScore =
        if kw `T.isInfixOf` T.toLower (unCategory ruleCategory)
          then wCategory
          else 0.0
   in titleScore + contentScore + tagScore + categoryScore

-- | Check if keyword matches a tag
keywordMatchesTag :: Text -> Tag -> Bool
keywordMatchesTag kw (Tag tag) = kw `T.isInfixOf` T.toLower tag

-- | Count occurrences of a substring in text
countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack
  | T.null needle = 0
  | otherwise = go 0 haystack
  where
    go count text
      | T.null text = count
      | needle `T.isPrefixOf` text = go (count + 1) (T.drop 1 text)
      | otherwise = go count (T.drop 1 text)

-- | Calculate similarity between two texts (for fuzzy matching)
textSimilarity :: Text -> Text -> Double
textSimilarity t1 t2 =
  let common = T.length $ T.filter (`T.elem` t2) t1
      total = max (T.length t1) (T.length t2)
   in if total == 0 then 0.0 else fromIntegral common / fromIntegral total
