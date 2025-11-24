{-# LANGUAGE OverloadedStrings #-}

module RpgRuleset.Query.Filter
  ( filterByVisibility,
    filterByUserRole,
    isVisibleToRole,
  )
where

import RpgRuleset.Core.Rule
import RpgRuleset.Core.Types

-- | Filter rules based on visibility setting
filterByVisibility :: Visibility -> [Rule] -> [Rule]
filterByVisibility GMOnly = id -- GM visibility includes everything
filterByVisibility Public = filter ((== Public) . ruleVisibility)

-- | Filter rules based on user role
-- Players only see Public rules, GMs see everything
filterByUserRole :: UserRole -> [Rule] -> [Rule]
filterByUserRole GameMaster = id -- GMs see all rules
filterByUserRole Player = filter ((== Public) . ruleVisibility)

-- | Check if a rule is visible to a user role
isVisibleToRole :: UserRole -> Rule -> Bool
isVisibleToRole GameMaster _ = True
isVisibleToRole Player rule = ruleVisibility rule == Public
