# Validation Checklists

**Feature**: RPG Ruleset Query System
**Version**: 1.0.0
**Related**: [data-model.md](../data-model.md), [cli-interface.md](./cli-interface.md)

## Overview

This document defines category-specific validation checklists used when creating or modifying rules. Each category has automated and manual checks to ensure rule quality and consistency.

## General Validation Checklist

Applied to all rules regardless of category.

```yaml
checklistId: general-validation
category: all
items:
  - id: unique-rule-id
    description: "Rule ID is unique within the system"
    severity: error
    autoCheckable: true

  - id: valid-version-format
    description: "Version follows semantic versioning (major.minor.patch)"
    severity: error
    autoCheckable: true

  - id: changelog-present
    description: "At least one changelog entry exists"
    severity: error
    autoCheckable: true

  - id: changelog-version-match
    description: "Latest changelog version matches rule version"
    severity: warning
    autoCheckable: true

  - id: valid-category
    description: "Category is one of: character-creation, world-building, interactions (or subcategories)"
    severity: error
    autoCheckable: true

  - id: tags-present
    description: "At least one tag is specified"
    severity: warning
    autoCheckable: true

  - id: tag-similarity
    description: "No highly similar tags (e.g., 'magic' vs 'magick')"
    severity: warning
    autoCheckable: true
    threshold: 0.85

  - id: related-rules-exist
    description: "All referenced related rules exist in the system"
    severity: warning
    autoCheckable: true

  - id: cross-system-refs-valid
    description: "Cross-system references point to existing systems and rules"
    severity: warning
    autoCheckable: true

  - id: visibility-specified
    description: "Visibility field is explicitly set (public or gm-only)"
    severity: error
    autoCheckable: true

  - id: content-not-empty
    description: "Rule content is not empty"
    severity: error
    autoCheckable: true

  - id: no-dangerous-characters
    description: "Formulas and conditions do not contain dangerous characters ($, ;, \\)"
    severity: error
    autoCheckable: true
```

## Character Creation Checklist

For rules in the `character-creation` category.

```yaml
checklistId: character-creation-validation
category: character-creation
inherits: general-validation
items:
  - id: balance-review
    description: "Rule has been reviewed for mechanical balance"
    severity: warning
    autoCheckable: false
    guidance: |
      Consider:
      - Does this rule make one option strictly better than others?
      - Are there opportunity costs for using this rule?
      - Does it interact unexpectedly with existing mechanics?

  - id: attribute-references-valid
    description: "All attribute references exist in the system (STR, DEX, etc.)"
    severity: error
    autoCheckable: true
    note: "Requires attribute schema definition in system metadata"

  - id: level-requirements-reasonable
    description: "Level requirements in conditions are reasonable (1-20 for most systems)"
    severity: warning
    autoCheckable: true

  - id: formula-syntax-valid
    description: "Formulas use valid syntax (dice notation, operators, variables)"
    severity: error
    autoCheckable: true

  - id: dice-notation-valid
    description: "Dice notation follows XdY format (e.g., 1d6, 2d20)"
    severity: error
    autoCheckable: true
    pattern: "\\d+d\\d+"

  - id: no-power-creep
    description: "Rule does not introduce power creep compared to similar rules"
    severity: warning
    autoCheckable: false
    guidance: |
      Compare with similar rules:
      - Is the benefit significantly stronger than existing options?
      - Does it make existing content obsolete?

  - id: condition-references-valid
    description: "Conditions reference valid character properties"
    severity: error
    autoCheckable: true

  - id: clear-prerequisites
    description: "Prerequisites are clearly stated in conditions or content"
    severity: warning
    autoCheckable: false

  - id: ancestry-constraints
    description: "Ancestry/race-specific rules are tagged appropriately"
    severity: warning
    autoCheckable: true
    tags: ["ancestry", "race"]

  - id: class-constraints
    description: "Class-specific rules are tagged appropriately"
    severity: warning
    autoCheckable: true
    tags: ["class"]
```

## World Building Checklist

For rules in the `world-building` category.

```yaml
checklistId: world-building-validation
category: world-building
inherits: general-validation
items:
  - id: geographic-consistency
    description: "Geographic references are consistent with established world geography"
    severity: warning
    autoCheckable: false
    guidance: |
      Check:
      - Are location names consistent with naming conventions?
      - Do distances and travel times make sense?
      - Are climate zones geographically plausible?

  - id: lore-consistency
    description: "Rule is consistent with established lore and history"
    severity: warning
    autoCheckable: false
    guidance: |
      Verify:
      - Does this contradict existing timeline events?
      - Are cultural references accurate?
      - Does it fit the world's tone and theme?

  - id: faction-references
    description: "Faction/organization references are valid"
    severity: warning
    autoCheckable: true
    note: "Requires faction registry in system metadata"

  - id: magic-system-consistency
    description: "Magic rules are consistent with system's magic framework"
    severity: error
    autoCheckable: false
    guidance: |
      Consider:
      - Does this follow established magic source rules?
      - Are costs and limits consistent with other spells?
      - Does it respect magic availability in the setting?

  - id: historical-references
    description: "Historical event references are valid"
    severity: warning
    autoCheckable: true
    note: "Requires timeline in system metadata"

  - id: scale-appropriate
    description: "Rule scope is appropriate (local, regional, continental, planar)"
    severity: info
    autoCheckable: false

  - id: environmental-effects
    description: "Environmental effects have clear mechanics"
    severity: warning
    autoCheckable: false
    guidance: |
      Ensure:
      - Effects on characters are specified
      - Duration and conditions are clear
      - Interactions with equipment/abilities defined

  - id: resource-availability
    description: "Resource scarcity/availability is defined"
    severity: info
    autoCheckable: false
```

## Interactions Checklist

For rules in the `interactions` category.

```yaml
checklistId: interactions-validation
category: interactions
inherits: general-validation
items:
  - id: action-economy-defined
    description: "Action economy is clearly defined (action, bonus action, reaction, etc.)"
    severity: error
    autoCheckable: false
    guidance: |
      Specify:
      - What type of action does this require?
      - Can it be combined with other actions?
      - Are there timing restrictions?

  - id: opposed-check-mechanics
    description: "Opposed checks have clear resolution mechanics"
    severity: error
    autoCheckable: false
    guidance: |
      Define:
      - What attributes/skills are used?
      - How are ties resolved?
      - Are there situational modifiers?

  - id: social-mechanics-clear
    description: "Social interaction mechanics are clearly defined"
    severity: warning
    autoCheckable: false
    guidance: |
      For social rules, specify:
      - Mechanical effects (bonuses, disadvantages)
      - Roleplay expectations
      - Limits and boundaries

  - id: combat-integration
    description: "Combat rules integrate with existing combat system"
    severity: error
    autoCheckable: false
    guidance: |
      Verify:
      - Initiative order is respected
      - Attack/damage formulas are consistent
      - Conditions use standard definitions

  - id: range-specified
    description: "Range/reach is specified for distance-dependent effects"
    severity: warning
    autoCheckable: true
    keywords: ["range", "reach", "distance", "feet", "meters"]

  - id: duration-specified
    description: "Duration is specified for time-dependent effects"
    severity: warning
    autoCheckable: true
    keywords: ["duration", "rounds", "minutes", "hours"]

  - id: concentration-effects
    description: "Concentration requirements are specified if applicable"
    severity: warning
    autoCheckable: true
    keywords: ["concentration"]

  - id: npc-interaction-rules
    description: "NPC interaction rules are clear and balanced"
    severity: warning
    autoCheckable: false
    guidance: |
      Consider:
      - Are NPC reactions predictable?
      - Do players have agency?
      - Are there fail-forward options?

  - id: skill-check-dcs
    description: "Skill check DCs are specified and reasonable"
    severity: warning
    autoCheckable: true
    guidance: |
      Typical DCs:
      - Easy: 5-10
      - Medium: 10-15
      - Hard: 15-20
      - Very Hard: 20-25
      - Nearly Impossible: 25+

  - id: passive-effects-clear
    description: "Passive effects vs active abilities are clearly distinguished"
    severity: info
    autoCheckable: false
```

## Automated Check Implementations

### Tag Similarity Check

```haskell
-- RpgRuleset/Validation/CategorySpecific.hs
checkTagSimilarity :: Rule -> [ValidationIssue]
checkTagSimilarity rule =
  let tags = Set.toList (rTags rule)
      pairs = [(t1, t2) | t1 <- tags, t2 <- tags, t1 < t2]
      similar = filter (\(Tag t1, Tag t2) -> similarity t1 t2 > 0.85) pairs
  in [ ValidationIssue
         { viCheckId = "tag-similarity"
         , viSeverity = Warning
         , viMessage = "Similar tags detected: " <> show t1 <> " and " <> show t2
         }
     | (t1, t2) <- similar
     ]

-- Levenshtein distance-based similarity
similarity :: Text -> Text -> Double
similarity t1 t2 =
  let dist = levenshteinDistance t1 t2
      maxLen = max (T.length t1) (T.length t2)
  in 1.0 - (fromIntegral dist / fromIntegral maxLen)
```

### Related Rules Exist Check

```haskell
checkRelatedRulesExist :: System -> Rule -> [ValidationIssue]
checkRelatedRulesExist system rule =
  let allRuleIds = Set.fromList (Map.keys (sysRules system))
      missing = filter (`Set.notMember` allRuleIds) (rRelatedRules rule)
  in [ ValidationIssue
         { viCheckId = "related-rules-exist"
         , viSeverity = Warning
         , viMessage = "Related rule not found: " <> show rid
         }
     | rid <- missing
     ]
```

### Dice Notation Validation

```haskell
checkDiceNotation :: Rule -> [ValidationIssue]
checkDiceNotation rule =
  case rFormulas rule of
    Nothing -> []
    Just formulas ->
      let issues = concatMap (checkFormulaDice . snd) (Map.toList formulas)
      in issues
  where
    checkFormulaDice :: Text -> [ValidationIssue]
    checkFormulaDice formula =
      let dicePatterns = T.splitOn " " formula
          invalidDice = filter (not . isValidDice) dicePatterns
      in [ ValidationIssue
             { viCheckId = "dice-notation-valid"
             , viSeverity = Error
             , viMessage = "Invalid dice notation: " <> d
             }
         | d <- invalidDice, "d" `T.isInfixOf` d
         ]

    isValidDice :: Text -> Bool
    isValidDice t = case T.splitOn "d" t of
      [x, y] -> T.all isDigit x && T.all isDigit y && not (T.null x) && not (T.null y)
      _ -> True  -- Not dice notation, ignore
```

### Attribute Reference Validation

```haskell
-- Requires system metadata with attribute schema
checkAttributeReferences :: SystemMetadata -> Rule -> [ValidationIssue]
checkAttributeReferences sysMeta rule =
  let validAttrs = Set.fromList (systemAttributes sysMeta)
      formulaAttrs = extractAttributes (rFormulas rule)
      conditionAttrs = extractAttributes (rConditions rule)
      allAttrs = formulaAttrs <> conditionAttrs
      invalid = filter (`Set.notMember` validAttrs) allAttrs
  in [ ValidationIssue
         { viCheckId = "attribute-references-valid"
         , viSeverity = Error
         , viMessage = "Invalid attribute reference: " <> attr
         }
     | attr <- invalid
     ]

extractAttributes :: Maybe (Map Text Text) -> [Text]
extractAttributes = -- Parse formulas/conditions for attribute references
```

## Validation Result Format

```haskell
-- RpgRuleset/Validation/Checklist.hs
data ValidationResult = ValidationResult
  { vrRuleId :: RuleId
  , vrChecklistId :: ChecklistId
  , vrIssues :: [ValidationIssue]
  , vrStatus :: ValidationStatus
  }
  deriving (Show, Eq)

data ValidationIssue = ValidationIssue
  { viCheckId :: Text
  , viSeverity :: Severity
  , viMessage :: Text
  }
  deriving (Show, Eq)

data Severity = Error | Warning | Info
  deriving (Show, Eq, Ord, Enum, Bounded)

data ValidationStatus = AllPassed | HasWarnings | HasErrors
  deriving (Show, Eq, Ord, Enum, Bounded)

determineStatus :: [ValidationIssue] -> ValidationStatus
determineStatus issues
  | any (\vi -> viSeverity vi == Error) issues = HasErrors
  | any (\vi -> viSeverity vi == Warning) issues = HasWarnings
  | otherwise = AllPassed
```

## CLI Output Example

```
Validating: rulesets/systems/my-rpg/character-creation/combat.md
Category: character-creation/combat
Checklist: character-creation-validation (inherits: general-validation)

Running automated checks...

✓ [PASS] unique-rule-id
✓ [PASS] valid-version-format
✓ [PASS] changelog-present
⚠ [WARNING] changelog-version-match
    Latest changelog version (2.0.0) does not match rule version (2.1.0)

✓ [PASS] valid-category
✓ [PASS] tags-present
⚠ [WARNING] tag-similarity
    Similar tags detected: "magic" (90% similar to "magick")

✓ [PASS] related-rules-exist
✓ [PASS] visibility-specified
✓ [PASS] content-not-empty
✓ [PASS] no-dangerous-characters
✓ [PASS] attribute-references-valid
✓ [PASS] formula-syntax-valid
✓ [PASS] dice-notation-valid

Manual review required:

⚪ [MANUAL] balance-review
    Rule has been reviewed for mechanical balance
    Guidance: Consider power level compared to similar rules

⚪ [MANUAL] no-power-creep
    Rule does not introduce power creep
    Compare with: COMBAT-1.1, COMBAT-1.3

Validation complete:
  ✓ 11 passed
  ✗ 0 errors
  ⚠ 2 warnings
  ⚪ 2 manual checks required

Status: PASS (warnings present)

Next steps:
  1. Address warnings if possible
  2. Complete manual reviews
  3. Commit changes with validation results
```

## Future Extensions

### Phase 2c: Formula Validation
- Parse formulas into AST
- Validate variable references against system schema
- Check for circular dependencies
- Detect unreachable conditions

### Post-MVP: Machine Learning
- Learn from validated rules to suggest tags
- Detect anomalies (rules that don't fit patterns)
- Suggest related rules based on content similarity

### Post-MVP: Cross-System Validation
- Validate variant rule compatibility with base system
- Detect breaking changes in system inheritance
- Suggest migration paths for incompatible rules

---

## References

- [Data Model](../data-model.md) - ValidationChecklist entity
- [CLI Interface](./cli-interface.md) - `validate` command
- [Research](../research.md) - Technology choices
