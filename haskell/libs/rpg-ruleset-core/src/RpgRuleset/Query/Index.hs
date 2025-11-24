{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RpgRuleset.Query.Index
  ( -- * Index Types
    QueryIndex (..),
    emptyIndex,

    -- * Building Indexes
    buildIndex,
    addRuleToIndex,
    addSystemToIndex,

    -- * Querying
    lookupRuleById,
    getRulesByCategory,
    getRulesBySystem,
    getRulesByTag,
    getPublicRules,
    getAllRules,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import RpgRuleset.Core.Rule
import RpgRuleset.Core.System (System, systemId)
import qualified RpgRuleset.Core.System as System
import RpgRuleset.Core.Types

-- | Query index for fast rule lookups
data QueryIndex = QueryIndex
  { -- | All rules indexed by ID
    qiAllRules :: !(Map RuleId Rule),
    -- | Rules grouped by category
    qiByCategory :: !(Map Category (Set RuleId)),
    -- | Rules grouped by system
    qiBySystem :: !(Map SystemId (Set RuleId)),
    -- | Rules grouped by tag
    qiByTag :: !(Map Tag (Set RuleId)),
    -- | Set of public (non-GM-only) rules
    qiPublicRules :: !(Set RuleId),
    -- | Loaded systems
    qiSystems :: !(Map SystemId System)
  }
  deriving (Show, Eq)

-- | Empty query index
emptyIndex :: QueryIndex
emptyIndex =
  QueryIndex
    { qiAllRules = Map.empty,
      qiByCategory = Map.empty,
      qiBySystem = Map.empty,
      qiByTag = Map.empty,
      qiPublicRules = Set.empty,
      qiSystems = Map.empty
    }

-- | Build an index from a list of systems
buildIndex :: [System] -> QueryIndex
buildIndex = foldl addSystemToIndex emptyIndex

-- | Add a system to the index
addSystemToIndex :: QueryIndex -> System -> QueryIndex
addSystemToIndex idx sys =
  let rules = System.getAllRules sys
      idx' = foldl addRuleToIndex idx rules
   in idx' {qiSystems = Map.insert (systemId sys) sys (qiSystems idx')}

-- | Add a single rule to the index
addRuleToIndex :: QueryIndex -> Rule -> QueryIndex
addRuleToIndex idx rule =
  let rid = ruleId rule
      cat = ruleCategory rule
      sid = ruleSystemId rule
      tags = ruleTags rule
      vis = ruleVisibility rule

      -- Update all rules map
      allRules' = Map.insert rid rule (qiAllRules idx)

      -- Update category index
      byCategory' = Map.insertWith Set.union cat (Set.singleton rid) (qiByCategory idx)

      -- Update system index
      bySystem' = Map.insertWith Set.union sid (Set.singleton rid) (qiBySystem idx)

      -- Update tag index
      byTag' =
        foldl
          (\m tag -> Map.insertWith Set.union tag (Set.singleton rid) m)
          (qiByTag idx)
          (Set.toList tags)

      -- Update public rules set
      publicRules' =
        if vis == Public
          then Set.insert rid (qiPublicRules idx)
          else qiPublicRules idx
   in idx
        { qiAllRules = allRules',
          qiByCategory = byCategory',
          qiBySystem = bySystem',
          qiByTag = byTag',
          qiPublicRules = publicRules'
        }

-- | Look up a rule by ID
lookupRuleById :: RuleId -> QueryIndex -> Maybe Rule
lookupRuleById rid = Map.lookup rid . qiAllRules

-- | Get all rules in a category
getRulesByCategory :: Category -> QueryIndex -> [Rule]
getRulesByCategory cat idx =
  let rids = Map.findWithDefault Set.empty cat (qiByCategory idx)
   in mapMaybe (`Map.lookup` qiAllRules idx) (Set.toList rids)
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- | Get all rules in a system
getRulesBySystem :: SystemId -> QueryIndex -> [Rule]
getRulesBySystem sid idx =
  let rids = Map.findWithDefault Set.empty sid (qiBySystem idx)
   in mapMaybe (`Map.lookup` qiAllRules idx) (Set.toList rids)
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- | Get all rules with a specific tag
getRulesByTag :: Tag -> QueryIndex -> [Rule]
getRulesByTag tag idx =
  let rids = Map.findWithDefault Set.empty tag (qiByTag idx)
   in mapMaybe (`Map.lookup` qiAllRules idx) (Set.toList rids)
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- | Get all public rules (visible to players)
getPublicRules :: QueryIndex -> [Rule]
getPublicRules idx =
  let rids = qiPublicRules idx
   in mapMaybe (`Map.lookup` qiAllRules idx) (Set.toList rids)
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- | Get all rules in the index
getAllRules :: QueryIndex -> [Rule]
getAllRules = Map.elems . qiAllRules
