{-# LANGUAGE OverloadedStrings #-}

module RpgRuleset.Validation.RuleId
  ( -- * Format Validation
    validateRuleIdFormat
  , checkRuleIdFormat
  , RuleIdError(..)
  , RuleIdWarning(..)
    -- * Duplicate Checking
  , checkDuplicateIds
    -- * Prefix Conventions
  , checkPrefixConventions
  , suggestedPrefixFor
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import RpgRuleset.Core.Types (RuleId(..), Category(..))

-- | Errors from rule ID validation (blocking)
data RuleIdError
  = InvalidFormat !RuleId !Text  -- ^ Rule ID doesn't match ^[A-Z]{2,6}-\d{3}$
  | DuplicateId !RuleId          -- ^ Rule ID already exists in system
  deriving (Show, Eq)

-- | Warnings from rule ID validation (non-blocking)
data RuleIdWarning
  = NonStandardPrefix !RuleId !Text !Text  -- ^ (id, used prefix, suggested prefix)
  deriving (Show, Eq)

-- | Validate rule ID format: ^[A-Z]{2,6}-\d{3}$
-- Returns Left error if invalid, Right () if valid
checkRuleIdFormat :: RuleId -> Either RuleIdError ()
checkRuleIdFormat rid@(RuleId ruleText) =
  case T.splitOn "-" ruleText of
    [prefix, num] ->
      let prefixLen = T.length prefix
          isUpperAlpha = T.all (`elem` ['A'..'Z']) prefix
          isThreeDigits = T.length num == 3 && T.all (`elem` ['0'..'9']) num
          validPrefix = prefixLen >= 2 && prefixLen <= 6 && isUpperAlpha
      in if validPrefix && isThreeDigits
         then Right ()
         else Left $ InvalidFormat rid (describeError prefix num)
    _ -> Left $ InvalidFormat rid "Rule ID must be in format PREFIX-NNN (e.g., CHAR-001)"
  where
    describeError prefix num
      | T.length prefix < 2 = "Prefix too short (minimum 2 characters)"
      | T.length prefix > 6 = "Prefix too long (maximum 6 characters)"
      | not (T.all (`elem` ['A'..'Z']) prefix) = "Prefix must be uppercase letters only"
      | T.length num /= 3 = "Number must be exactly 3 digits"
      | not (T.all (`elem` ['0'..'9']) num) = "Number must contain only digits"
      | otherwise = "Invalid format"

-- | Simple validation returning Bool (for backwards compatibility)
validateRuleIdFormat :: RuleId -> Bool
validateRuleIdFormat rid = case checkRuleIdFormat rid of
  Right () -> True
  Left _ -> False

-- | Check for duplicate IDs in a set of existing IDs
-- Returns Left error if duplicate found, Right () otherwise
checkDuplicateIds :: RuleId -> Set RuleId -> Either RuleIdError ()
checkDuplicateIds rid existingIds
  | rid `Set.member` existingIds = Left $ DuplicateId rid
  | otherwise = Right ()

-- | Check if rule ID uses a standard prefix for its category
-- Returns warnings if non-standard prefix is used
checkPrefixConventions :: RuleId -> Category -> [RuleIdWarning]
checkPrefixConventions rid@(RuleId ruleText) cat =
  case T.splitOn "-" ruleText of
    (prefix : _) ->
      let suggested = suggestedPrefixFor cat
      in if prefix `elem` suggested
         then []
         else case suggested of
           (s:_) -> [NonStandardPrefix rid prefix s]
           [] -> []
    _ -> []

-- | Get suggested prefixes for a category
suggestedPrefixFor :: Category -> [Text]
suggestedPrefixFor (Category cat) = case cat of
  "character-creation" -> ["CHAR", "CLASS", "RACE", "CHRGEN"]
  "world-building"     -> ["WRLD", "GEOG", "SETTL", "REGION"]
  "interactions"       -> ["INTR", "SOCL", "CMBT", "SKILL"]
  _                    -> ["MISC", "RULE"]

-- | Extract prefix from a rule ID
extractPrefix :: RuleId -> Maybe Text
extractPrefix (RuleId ruleText) =
  case T.splitOn "-" ruleText of
    (prefix : _) -> Just prefix
    _ -> Nothing

-- | Generate next available rule ID for a prefix
suggestNextId :: Text -> Set RuleId -> RuleId
suggestNextId prefix existingIds =
  let matching = Set.filter (hasPrefix prefix) existingIds
      numbers = Set.map extractNumber matching
      maxNum = if Set.null numbers then 0 else Set.findMax numbers
      nextNum = maxNum + 1
      paddedNum = T.justifyRight 3 '0' (T.pack $ show nextNum)
  in RuleId $ prefix <> "-" <> paddedNum
  where
    hasPrefix pfx (RuleId rt) = pfx `T.isPrefixOf` rt
    extractNumber (RuleId rt) =
      case T.splitOn "-" rt of
        [_, numText] -> maybe 0 id (readMaybe (T.unpack numText))
        _ -> 0
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing