# Data Model: RPG Ruleset Query System

**Feature**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md) | **Research**: [research.md](./research.md)
**Date**: 2025-11-16
**Status**: Complete

## Overview

This document defines all entities, relationships, and data structures for the RPG Ruleset Documentation & Query System. The model supports:

- **Multi-world architecture**: Systems, variants, and worlds
- **Rich rule metadata**: 17 clarification features implemented
- **System inheritance**: Base systems extended by variants
- **Conditional & calculated rules**: Structured formula support
- **Access control**: GM-only visibility filtering
- **Content portability**: Export/import with validation

## Core Entities

### 1. Rule

The fundamental unit representing a single game rule or mechanic.

```haskell
-- RpgRuleset/Core/Rule.hs
module RpgRuleset.Core.Rule where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON, ToJSON)

data Rule = Rule
  { -- Identity (FR-002, FR-003)
    ruleId :: RuleId
  , category :: Category
  , systemId :: SystemId

  -- Content (FR-001)
  , title :: Maybe Text              -- Optional title for rule
  , content :: MarkdownContent       -- Full rule text in Markdown

  -- Metadata (FR-004, FR-005)
  , tags :: Set Tag
  , visibility :: Visibility         -- FR-028 (public | gm-only)

  -- Versioning (FR-029, Clarification 11)
  , version :: Version
  , changelog :: [ChangelogEntry]

  -- Relationships (FR-006)
  , relatedRules :: [RuleId]
  , crossSystemRefs :: [CrossSystemRef]  -- FR-022

  -- Advanced Features (Clarifications 12-13)
  , conditions :: Maybe [Condition]       -- FR-026
  , formulas :: Maybe (Map FormulaName Formula)  -- FR-027

  -- File metadata
  , sourceFile :: FilePath
  , loadedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON Rule
instance ToJSON Rule

-- Type aliases for clarity
newtype RuleId = RuleId Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

newtype Category = Category Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

newtype Tag = Tag Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

data Visibility
  = Public    -- Visible to all players
  | GMOnly    -- Only visible to Game Master
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

type MarkdownContent = Text
```

**Invariants**:
- `ruleId` MUST be unique within a system (enforced at load time)
- `category` MUST match one of the three core categories or subcategories
- `version` MUST follow semantic versioning (major.minor.patch)
- `relatedRules` references MUST exist in the same system or be cross-system refs

### 2. System

Represents a complete RPG ruleset (base or variant).

```haskell
-- RpgRuleset/Core/System.hs
module RpgRuleset.Core.System where

import Data.Text (Text)
import Data.Map.Strict (Map)

data System = System
  { -- Identity
    systemId :: SystemId
  , name :: Text
  , description :: Maybe Text

  -- Inheritance (FR-021, Clarification 7)
  , systemType :: SystemType
  , baseSystem :: Maybe SystemId     -- For variants only

  -- Content
  , rules :: Map RuleId Rule

  -- Metadata
  , version :: Version
  , authors :: [Text]
  , createdAt :: UTCTime
  , modifiedAt :: UTCTime

  -- Directory structure
  , rootPath :: FilePath
  }
  deriving (Show, Eq, Generic)

newtype SystemId = SystemId Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

data SystemType
  = BaseSystem      -- Original, standalone ruleset
  | VariantSystem   -- Extends a base system (FR-021)
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

-- System metadata stored in system.yaml at root
data SystemMetadata = SystemMetadata
  { metaSystemId :: SystemId
  , metaName :: Text
  , metaDescription :: Maybe Text
  , metaSystemType :: SystemType
  , metaBaseSystem :: Maybe SystemId
  , metaVersion :: Version
  , metaAuthors :: [Text]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
```

**Directory Structure Example**:
```
systems/
├── my-fantasy-rpg/           # Base system
│   ├── system.yaml           # System metadata
│   ├── character-creation/
│   ├── world-building/
│   └── interactions/
└── my-fantasy-rpg-variant/   # Variant system
    ├── system.yaml           # Includes baseSystem: "my-fantasy-rpg"
    ├── character-creation/   # Override/extend base rules
    └── interactions/         # Add new rules
```

**Invariants**:
- If `systemType == VariantSystem`, then `baseSystem` MUST be `Just baseId`
- If `systemType == BaseSystem`, then `baseSystem` MUST be `Nothing`
- Variant systems MUST NOT create circular inheritance (enforced at load time)

### 3. World

Represents a game world that uses one or more systems.

```haskell
-- RpgRuleset/Core/World.hs
module RpgRuleset.Core.World where

import Data.Text (Text)
import Data.Set (Set)

data World = World
  { -- Identity
    worldId :: WorldId
  , name :: Text
  , description :: Maybe Text

  -- System relationships (FR-023, Clarification 8)
  , primarySystem :: SystemId
  , additionalSystems :: Set SystemId  -- For multi-system worlds

  -- Content (future: characters, campaigns)
  , characters :: [CharacterRef]       -- Placeholder for future

  -- Metadata
  , createdAt :: UTCTime
  , modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

newtype WorldId = WorldId Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

-- Placeholder for future character support
data CharacterRef = CharacterRef
  { characterId :: Text
  , characterFile :: FilePath
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
```

**Invariants**:
- `primarySystem` MUST exist in loaded systems
- All `additionalSystems` MUST exist in loaded systems
- Systems in a world MUST NOT have conflicting rule IDs (warning, not error)

### 4. Version & Changelog

Version tracking for rules and systems (Clarification 11).

```haskell
-- RpgRuleset/Core/Types.hs
data Version = Version
  { versionMajor :: Int
  , versionMinor :: Int
  , versionPatch :: Int
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Display Version where
  display (Version maj min pat) = show maj <> "." <> show min <> "." <> show pat

data ChangelogEntry = ChangelogEntry
  { changeVersion :: Version
  , changeDate :: UTCTime
  , changeDescription :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
```

### 5. Condition & Formula

Structured representations for conditional and calculated rules (Clarifications 12-13).

```haskell
-- RpgRuleset/Core/Types.hs

-- Phase 1 (MVP): Simple text storage
type Condition = Text  -- e.g., "character.weapon_equipped == true"
type Formula = Text    -- e.g., "1d6 + STR_modifier"
type FormulaName = Text  -- e.g., "attack_roll", "damage"

-- Phase 2 (Advanced Rules): Parsed AST (future)
data ConditionAST
  = CompareOp ComparisonOp Expr Expr
  | LogicalOp LogicalOp ConditionAST ConditionAST
  | NotOp ConditionAST
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Expr
  = VarRef Text             -- character.level
  | LiteralInt Int
  | LiteralBool Bool
  | LiteralText Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ComparisonOp = Equals | NotEquals | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

data LogicalOp = And | Or
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

data FormulaAST
  = DiceRoll Int Int        -- XdY (e.g., 1d6)
  | Variable Text           -- STR_modifier
  | Constant Int
  | BinOp ArithOp FormulaAST FormulaAST
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data ArithOp = Add | Subtract | Multiply | Divide
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)
```

### 6. CrossSystemRef

References to rules in other systems (FR-022, Clarification 7).

```haskell
-- RpgRuleset/Core/Types.hs
data CrossSystemRef = CrossSystemRef
  { targetSystem :: SystemId
  , targetRule :: RuleId
  , refType :: RefType
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RefType
  = Extends      -- Variant rule extends base rule
  | Replaces     -- Variant rule replaces base rule
  | References   -- Informational reference
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)
```

**Invariants**:
- Cross-system references MUST be validated during system load
- Circular references are allowed but generate warnings
- Missing references are warnings (not errors) for content portability

### 7. Query

Represents a search query for rules.

```haskell
-- RpgRuleset/Query/Engine.hs
module RpgRuleset.Query.Engine where

import Data.Text (Text)
import Data.Set (Set)

data Query = Query
  { -- Search terms
    keywords :: [Text]

  -- Filters
  , filterCategory :: Maybe Category
  , filterSystem :: Maybe SystemId
  , filterTags :: Set Tag
  , filterVisibility :: Maybe Visibility  -- FR-028

  -- Pagination
  , limit :: Maybe Int
  , offset :: Int
  }
  deriving (Show, Eq, Generic)

data QueryResult = QueryResult
  { matchedRules :: [ScoredRule]
  , totalMatches :: Int
  , queryTime :: Double  -- seconds
  }
  deriving (Show, Eq, Generic)

data ScoredRule = ScoredRule
  { rule :: Rule
  , score :: Double
  , matchHighlights :: [MatchHighlight]
  }
  deriving (Show, Eq, Generic)

data MatchHighlight = MatchHighlight
  { field :: MatchField
  , snippet :: Text
  }
  deriving (Show, Eq, Generic)

data MatchField
  = TitleMatch
  | ContentMatch
  | TagMatch
  | CategoryMatch
  deriving (Show, Eq, Enum, Bounded, Generic)
```

### 8. ValidationChecklist

Checklist workflow for validating new/modified rules (FR-007, Clarification 5).

```haskell
-- RpgRuleset/Validation/Checklist.hs
module RpgRuleset.Validation.Checklist where

import Data.Text (Text)
import Data.Map.Strict (Map)

data ValidationChecklist = ValidationChecklist
  { checklistId :: ChecklistId
  , forCategory :: Category
  , items :: [ChecklistItem]
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype ChecklistId = ChecklistId Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

data ChecklistItem = ChecklistItem
  { itemId :: Text
  , description :: Text
  , severity :: Severity
  , autoCheckable :: Bool  -- Can be automated vs manual review
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Severity
  = Error      -- Must fix before accepting rule
  | Warning    -- Should review but not blocking
  | Info       -- Informational only
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

-- Result of running validation
data ValidationResult = ValidationResult
  { validatedRule :: RuleId
  , checklistUsed :: ChecklistId
  , results :: Map Text CheckResult
  , overallStatus :: ValidationStatus
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CheckResult = CheckResult
  { checkItem :: Text
  , status :: CheckStatus
  , message :: Maybe Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CheckStatus = Pass | Fail | Skip | ManualReview
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)

data ValidationStatus = AllPassed | HasWarnings | HasErrors
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)
```

**Validation Checklist Examples** (see `contracts/validation-checklists.md`):
- **General**: Duplicate ID check, tag similarity, related rules exist
- **Character Creation**: Balance checks, attribute references, level requirements
- **World Building**: Geographic consistency, lore references
- **Interactions**: Action economy, opposed checks, social mechanics

### 9. ContentPackage

Export/import structure for content portability (FR-024, Clarification 9).

```haskell
-- RpgRuleset/Export/Import.hs
module RpgRuleset.Export.Import where

import Data.Text (Text)
import Data.Set (Set)

data ContentPackage = ContentPackage
  { -- Metadata
    packageId :: PackageId
  , packageName :: Text
  , description :: Maybe Text
  , version :: Version
  , createdAt :: UTCTime

  -- Source information
  , sourceSystem :: SystemId
  , sourceWorld :: Maybe WorldId

  -- Content
  , rules :: [Rule]
  , dependencies :: Set RuleId  -- External rules referenced

  -- Validation
  , validationWarnings :: [ValidationWarning]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype PackageId = PackageId Text
  deriving (Show, Eq, Ord, IsString, FromJSON, ToJSON)

data ValidationWarning = ValidationWarning
  { warningType :: WarningType
  , affectedRules :: [RuleId]
  , message :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data WarningType
  = MissingDependency
  | IncompatibleSystem
  | ConflictingRuleId
  | BrokenCrossRef
  deriving (Show, Eq, Enum, Bounded, Generic, FromJSON, ToJSON)
```

## Entity Relationships

### ER Diagram (Conceptual)

```
┌─────────────┐
│   World     │
│  (campaign) │
└──────┬──────┘
       │ 1
       │ uses
       │ 1..*
       ▼
┌─────────────┐        ┌─────────────┐
│   System    │◄───────│    Rule     │
│ (ruleset)   │ owns   │ (game rule) │
└──────┬──────┘ 1   *  └──────┬──────┘
       │                       │
       │ inherits from         │ related to
       │ (for variants)        │ (same/cross-system)
       ▼                       ▼
┌─────────────┐        ┌─────────────┐
│   System    │        │    Rule     │
│   (base)    │        │  (related)  │
└─────────────┘        └─────────────┘
```

### Relationship Details

**System → Rule** (Composition)
- A System owns many Rules
- Rules cannot exist without a System
- Deletion: Cascade (deleting system removes all its rules)

**System → System** (Inheritance)
- A Variant System extends one Base System
- Base Systems have no parent
- Constraint: No circular inheritance

**World → System** (Association)
- A World uses 1 primary System and 0..* additional Systems
- Systems can be used by multiple Worlds
- Deletion: World removed if system deleted (or make orphan check)

**Rule → Rule** (Association - Related Rules)
- A Rule can reference many related Rules (same system)
- Bidirectional relationship (not enforced)
- Deletion: Warning if related rules deleted

**Rule → Rule** (Association - Cross-System References)
- A Rule can reference Rules in other Systems (via CrossSystemRef)
- Used for variant overrides and informational links
- Deletion: Warning if cross-referenced rule deleted

## Data Storage Format

### File Organization

```
rulesets/
├── systems/
│   ├── my-fantasy-rpg/
│   │   ├── system.yaml                    # System metadata
│   │   ├── character-creation/
│   │   │   ├── attributes.md              # Multiple rules per file
│   │   │   ├── ancestries.md
│   │   │   └── classes/
│   │   │       ├── fighter.md
│   │   │       └── wizard.md
│   │   ├── world-building/
│   │   │   ├── geography.md
│   │   │   └── magic-system.md
│   │   └── interactions/
│   │       ├── combat.md
│   │       └── social.md
│   └── my-fantasy-rpg-variant/
│       ├── system.yaml                    # baseSystem: "my-fantasy-rpg"
│       └── character-creation/
│           └── classes/
│               └── fighter.md             # Override base fighter rules
└── worlds/
    ├── campaign-world-alpha/
    │   └── world.yaml                     # World metadata
    └── campaign-world-beta/
        └── world.yaml
```

### YAML Frontmatter Schema

See `contracts/rule-schema.yaml` for complete JSON Schema definition.

**Example Rule File**:
```markdown
---
category: character-creation/combat
system: my-fantasy-rpg
rules:
  - id: COMBAT-1.2
    version: 2.1
    changelog:
      - version: 2.1
        date: 2025-11-15T10:30:00Z
        changes: "Reduced critical multiplier from 3x to 2x for balance"
      - version: 2.0
        date: 2025-10-01T14:00:00Z
        changes: "Initial combat rule definition"
    tags: [mechanics, combat, attack, critical-hits]
    visibility: public
    related: [COMBAT-1.3, CHARACTER-0.2]
    conditions:
      - "character.weapon_equipped == true"
      - "character.level >= 1"
    formulas:
      attack_roll: "1d20 + STR_modifier + proficiency_bonus"
      critical_damage: "weapon_damage * 2 + STR_modifier"

  - id: COMBAT-1.3
    version: 1.0
    changelog:
      - version: 1.0
        date: 2025-10-01T14:00:00Z
        changes: "Initial definition"
    tags: [mechanics, combat, defense]
    visibility: public
    related: [COMBAT-1.2]
---

## Combat Rules

### [COMBAT-1.2] Critical Hits

When you roll a natural 20 on an attack roll, you score a **critical hit**.

**Effect**: Roll the weapon's damage dice twice and add modifiers once.

**Example**: Longsword (1d8 + STR) critical = 2d8 + STR modifier

**Formulas**:
- Attack roll: `1d20 + STR_modifier + proficiency_bonus`
- Critical damage: `weapon_damage * 2 + STR_modifier`

**Conditions**:
- Character must have a weapon equipped
- Applies to all weapon attacks

---

### [COMBAT-1.3] Armor Class

Your **Armor Class (AC)** represents how hard you are to hit...

[Rest of markdown content]
```

### system.yaml Schema

```yaml
systemId: my-fantasy-rpg
name: "My Fantasy RPG"
description: "A custom fantasy roleplaying system"
systemType: base  # or "variant"
# baseSystem: "parent-system-id"  # Only for variants
version:
  major: 1
  minor: 0
  patch: 0
authors:
  - "Game Designer Name"
createdAt: 2025-11-01T10:00:00Z
modifiedAt: 2025-11-15T14:30:00Z
```

### world.yaml Schema

```yaml
worldId: campaign-world-alpha
name: "Campaign World Alpha"
description: "First campaign setting"
primarySystem: my-fantasy-rpg
additionalSystems:
  - optional-expansion-system
createdAt: 2025-11-05T09:00:00Z
modifiedAt: 2025-11-10T16:45:00Z
```

## Data Flow Diagrams

### 1. Rule Loading Flow

```
┌─────────────┐
│ File System │
└──────┬──────┘
       │ 1. Scan directories
       ▼
┌──────────────────┐
│ FileSystem.Loader│
└──────┬───────────┘
       │ 2. Read .md files
       ▼
┌──────────────────┐
│ Parser.Yaml      │ 3. Parse frontmatter
│ Parser.Markdown  │ 4. Parse content
└──────┬───────────┘
       │ 5. Create Rule entities
       ▼
┌──────────────────┐
│ Core.System      │ 6. Build System with Rules
└──────┬───────────┘
       │ 7. Build indexes
       ▼
┌──────────────────┐
│ Query.Index      │ Ready for queries
└──────────────────┘
```

### 2. Query Execution Flow

```
┌──────────────┐
│ User Query   │ "combat critical"
└──────┬───────┘
       │ 1. Parse CLI args
       ▼
┌──────────────────┐
│ Query.Engine     │ 2. Build Query object
└──────┬───────────┘
       │ 3. Filter by category/system/tags
       ▼
┌──────────────────┐
│ Query.Filter     │ 4. Apply visibility rules
└──────┬───────────┘
       │ 5. Score remaining rules
       ▼
┌──────────────────┐
│ Query.Ranking    │ 6. Sort by score
└──────┬───────────┘
       │ 7. Format results
       ▼
┌──────────────────┐
│ CLI.Output       │ 8. Display to user
└──────────────────┘
```

### 3. Validation Workflow

```
┌──────────────────┐
│ New/Modified Rule│
└──────┬───────────┘
       │ 1. Load rule
       ▼
┌──────────────────────┐
│ Validation.Checklist │ 2. Select category checklist
└──────┬───────────────┘
       │ 3. Run automated checks
       ▼
┌──────────────────────────┐
│ Validation.CategorySpecific│ 4. Category-specific checks
└──────┬─────────────────────┘
       │ 5. Formula/condition validation
       ▼
┌──────────────────────┐
│ Validation.Formulas  │ 6. Syntax check
│ Validation.Conditions│ 7. Reference check
└──────┬───────────────┘
       │ 8. Generate report
       ▼
┌──────────────────────┐
│ ValidationResult     │ Pass/Warning/Error
└──────────────────────┘
```

## Indexing Strategy

### Primary Index
```haskell
-- O(log n) lookup by rule ID
type RuleIndex = Map RuleId Rule
```

### Secondary Indexes
```haskell
data QueryIndex = QueryIndex
  { -- Category index (O(log n) filter)
    rulesByCategory :: Map Category (Set RuleId)

  , -- System index (O(log n) filter)
    rulesBySystem :: Map SystemId (Set RuleId)

  , -- Tag index (O(log n) lookup, O(t) where t = tags per rule)
    rulesByTag :: Map Tag (Set RuleId)

  , -- Visibility index (O(1) filter)
    publicRules :: Set RuleId
  , gmOnlyRules :: Set RuleId

  , -- Primary data
    allRules :: RuleIndex
  }
```

### Index Build Time
- **Scan & parse**: O(n) where n = number of files
- **Index build**: O(n * log n) for Map insertions
- **Expected**: ~100ms for 1000 rules (file I/O dominates)

### Query Time Complexity
- **Filter by category**: O(log n) lookup in `rulesByCategory`
- **Filter by tag**: O(log n) lookup in `rulesByTag`, O(t) intersection
- **Score all**: O(m) where m = filtered rules (typically m << n)
- **Sort**: O(m log m)
- **Expected**: ~10ms for 1000 rules (in-memory operations)

## Data Validation Rules

### Load-Time Validation (Errors)
- Rule IDs MUST be unique within a system
- Category MUST be one of: character-creation, world-building, interactions (or subcategories)
- Version MUST be valid semantic version (major.minor.patch)
- System YAML MUST be present for each system directory
- Variant systems MUST reference existing base system (no circular inheritance)

### Load-Time Validation (Warnings)
- Related rules referenced but not found (might be in another system)
- Cross-system references to non-existent rules
- Tags with high similarity (e.g., "magic" vs "magick")
- Missing changelog entries
- Empty rule content

### Query-Time Validation
- Visibility filtering based on user role (GM vs player)
- System compatibility checks for multi-system queries

### Export-Time Validation
- Dependency completeness (all referenced rules included or flagged)
- Target system compatibility warnings
- Rule ID conflicts with destination

## Performance Characteristics

| Operation | Complexity | Expected Time (1000 rules) |
|-----------|------------|----------------------------|
| Load all rules | O(n) | ~100ms (I/O bound) |
| Build indexes | O(n log n) | ~10ms |
| Query by keyword | O(n) scan | ~5ms |
| Query by category | O(log n) + O(m) | ~2ms (m << n) |
| Query by tag | O(log n) + O(m) | ~2ms |
| Validate rule | O(r) (r = refs) | ~1ms |
| Export package | O(k) (k = rules in package) | ~10ms for 50 rules |

**Success Criteria Validation**:
- SC-001: Query < 2 seconds ✅ (actual ~10-20ms)
- SC-009: Validation < 2 minutes ✅ (actual ~5-10 seconds for full system)

## Future Extensions

### Phase 2b: Multi-World Support
- Add `Inheritance.Resolver` module for variant rule resolution
- Implement cross-system reference validation
- Add system compatibility matrix

### Phase 2c: Advanced Rules
- Parse formulas into `FormulaAST` (replace `Text` storage)
- Parse conditions into `ConditionAST`
- Add formula evaluation engine (for validation, not runtime gameplay)
- Implement dependency graph for formulas (detect circular refs)

### Phase 2d: Content Portability
- Implement `Export.Import` module
- Add rule mapping suggestions (ML-based, optional)
- Build compatibility checker for cross-system imports

### Future Enhancements (Post-MVP)
- **Character entity**: Link characters to worlds, track which rules apply
- **Campaign entity**: Group worlds and sessions
- **Rule history**: Query old versions of rules via git integration
- **Full-text search**: Integrate with ripgrep or similar for content search
- **Web interface**: Browser-based query UI (reuse `rpg-ruleset-core` library)
- **LSP server**: IDE integration for rule authoring with validation

## References

- [Spec](./spec.md) - User scenarios and requirements
- [Plan](./plan.md) - Implementation phases
- [Research](./research.md) - Technology choices
- [Contracts](./contracts/) - Interface specifications (next)

**Data model complete. Ready to proceed to contract definitions.**
