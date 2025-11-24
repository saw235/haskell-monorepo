# Research & Technology Validation: RPG Ruleset Query System

**Feature**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md)
**Date**: 2025-11-16
**Status**: Complete

## Executive Summary

This document resolves all technology choices for the RPG Ruleset Documentation & Query System. All decisions prioritize:
- **Simplicity**: Minimal dependencies, standard Haskell libraries
- **Maintainability**: Well-documented libraries with active communities
- **Performance**: Meeting success criteria (SC-001: query < 2 seconds, SC-009: validation < 2 minutes)
- **Bazel Integration**: Compatible with existing monorepo build infrastructure

## 1. YAML Frontmatter Parsing

### Options Evaluated

**Option A: `yaml` (Data.Yaml from Aeson)**
- **Pros**:
  - Already widely used in Haskell ecosystem
  - Excellent Aeson integration for JSON-like data structures
  - Strong error messages
  - Active maintenance
- **Cons**:
  - Slightly heavier dependency tree
  - Uses libyaml C library (FFI)
- **Performance**: Excellent for our scale (hundreds of rules)

**Option B: `HsYAML`**
- **Pros**:
  - Pure Haskell implementation (no FFI)
  - Smaller dependency footprint
  - YAML 1.2 spec compliant
- **Cons**:
  - Less mature ecosystem integration
  - Requires manual bridging to Aeson types
  - Slower for large files (not an issue at our scale)

### Decision: `yaml` (Data.Yaml)

**Rationale**:
- Seamless Aeson integration critical for rule metadata parsing
- Better error messages improve user experience when authoring rules
- C dependency acceptable in Bazel environment (constitutional compliance)
- Proven at scale in production Haskell projects

**Integration Pattern**:
```haskell
-- RpgRuleset/Parser/Yaml.hs
parseRuleFrontmatter :: ByteString -> Either String RuleMetadata
parseRuleFrontmatter = decodeEither'

data RuleMetadata = RuleMetadata
  { category :: Text
  , system :: Text
  , rules :: [RuleEntry]
  } deriving (Generic, FromJSON)
```

## 2. Markdown Parsing

### Options Evaluated

**Option A: `pandoc`**
- **Pros**:
  - Universal document converter, extremely powerful
  - Supports tables, formulas, many extensions
  - Can render to multiple formats (HTML, LaTeX, etc.)
- **Cons**:
  - **Very heavy dependency** (50+ transitive dependencies)
  - Overkill for our use case (we only need parsing, not conversion)
  - Slower startup time

**Option B: `cmark` (CommonMark binding)**
- **Pros**:
  - Fast C-based parser via FFI
  - CommonMark spec compliant
  - Lightweight (3-4 dependencies)
  - Excellent for GitHub-flavored markdown
- **Cons**:
  - Limited extension support
  - C dependency (acceptable per constitution)

**Option C: `commonmark-hs`**
- **Pros**:
  - Pure Haskell implementation
  - Extensible architecture
  - Good table support via `commonmark-extensions`
  - Active development
- **Cons**:
  - Slightly more complex API
  - Slower than cmark for very large documents (not our bottleneck)

### Decision: `cmark` with fallback to `commonmark-hs` if extensions needed

**Rationale**:
- **Initial MVP**: `cmark` for simplicity and speed
  - Handles core markdown parsing (headers, lists, code blocks)
  - Sufficient for rule content storage
  - Minimal dependency footprint

- **Future extension path**: Switch to `commonmark-hs` if we need:
  - Advanced table parsing
  - Custom syntax extensions (e.g., dice notation syntax highlighting)
  - Strikethrough, task lists, other GFM extensions

**Integration Pattern**:
```haskell
-- RpgRuleset/Parser/Markdown.hs
import CMarkGFM (commonmarkToNode, optSafe)

parseRuleContent :: Text -> Node
parseRuleContent md = commonmarkToNode [optSafe] md

extractTables :: Node -> [Table]
extractTables = -- traverse AST for table nodes
```

## 3. Formula & Condition DSL Parsing

### Requirements Analysis

From spec clarifications:
- **Formulas**: `"1d6 + STR_modifier"`, `"weapon_damage * 2"`
- **Conditions**: `"character.weapon_equipped == true"`, `"character.level >= 5"`

### Options Evaluated

**Option A: Simple string templates (no parsing)**
- **Pros**: Zero complexity, just store strings
- **Cons**: Cannot validate syntax, no error checking

**Option B: Full parser with `megaparsec`**
- **Pros**:
  - Excellent error messages
  - Composable parsers
  - Industry standard for Haskell parsing
  - Can validate formulas at rule creation time
- **Cons**:
  - Adds complexity
  - Need to design grammar

**Option C: `attoparsec`**
- **Pros**: Faster than megaparsec
- **Cons**: Worse error messages (critical for user-facing tool)

### Decision: **Two-phase approach**

**Phase 1 (MVP)**: Store formulas/conditions as `Text`, basic regex validation
```haskell
-- Simple validation only
validateFormula :: Text -> Either String ()
validateFormula f =
  if T.any (`elem` ['$', ';', '\\']) f
  then Left "Invalid characters in formula"
  else Right ()
```

**Phase 2 (Advanced Rules)**: Full `megaparsec` parser for validation
```haskell
-- RpgRuleset/Parser/Formula.hs
import Text.Megaparsec

data Formula
  = DiceRoll Int Int        -- XdY (e.g., 1d6)
  | Variable Text           -- e.g., STR_modifier
  | BinOp Op Formula Formula
  deriving (Show, Eq)

parseFormula :: Text -> Either String Formula
```

**Rationale**:
- MVP prioritizes delivery over perfect validation
- String storage provides flexibility as formula DSL evolves
- Megaparsec enables excellent user-facing error messages when validation added
- Constitutional test-driven approach: validation logic well-suited for QuickCheck properties

## 4. File System Traversal & Rule Loading

### Requirements
- Load all markdown files from directory hierarchy
- Respect category structure (character-creation/, world-building/, etc.)
- Handle missing files gracefully
- Efficient for 100-1000 files

### Options Evaluated

**Option A: `directory` + `filepath` (base libraries)**
- **Pros**:
  - No extra dependencies
  - Simple, well-understood APIs
  - Sufficient for our scale
- **Cons**:
  - Manual recursive traversal
  - Need to handle symlinks, permissions

**Option B: `path-io` or `filemanip`**
- **Pros**: Higher-level traversal utilities
- **Cons**: Extra dependencies for minimal benefit

### Decision: `directory` + `filepath` with structured traversal

**Rationale**:
- Zero additional dependencies (constitutional simplicity)
- Scale supports simple recursive approach
- Clear control flow for error handling
- Easy to test with fixture directories

**Implementation Pattern**:
```haskell
-- RpgRuleset/FileSystem/Loader.hs
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)

loadRulesFromDirectory :: FilePath -> IO [RuleFile]
loadRulesFromDirectory root = do
  entries <- listDirectory root
  concat <$> traverse (loadEntry root) entries
  where
    loadEntry dir entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then loadRulesFromDirectory path
        else if takeExtension entry == ".md"
          then (:[]) <$> loadRuleFile path
          else pure []
```

## 5. Query Ranking Algorithm

### Requirements
- Keyword matching (FR-001, FR-002)
- Category filtering (FR-003)
- Tag relevance (FR-004)
- < 2 seconds for typical queries (SC-001)

### Options Evaluated

**Option A: Simple keyword count + category boost**
- **Pros**:
  - Trivial to implement
  - Fast (O(n) scan)
  - Predictable results
- **Cons**:
  - No term frequency weighting
  - Cannot handle synonyms

**Option B: TF-IDF (Term Frequency-Inverse Document Frequency)**
- **Pros**:
  - Industry standard
  - Weights rare terms higher
  - Better relevance for large corpora
- **Cons**:
  - Requires pre-computing IDF scores
  - Overkill for hundreds of rules

**Option C: Hybrid scoring**
- **Pros**:
  - Combine keyword count + category match + tag match
  - Tunable weights
  - Room to grow
- **Cons**:
  - Slightly more complex

### Decision: **Hybrid scoring with simple weights**

**Rationale**:
- Our corpus size (100-1000 rules) doesn't justify TF-IDF complexity
- Category and tag matching are high-signal features
- Simple scoring meets performance requirements
- Can add TF-IDF later if needed (premature optimization avoided)

**Scoring Formula**:
```haskell
-- RpgRuleset/Query/Ranking.hs
data QueryScore = QueryScore
  { keywordMatches :: Int      -- weight: 1.0
  , categoryMatch :: Bool       -- weight: 10.0 (high signal)
  , tagMatches :: Int           -- weight: 2.0
  , titleMatch :: Bool          -- weight: 5.0
  }

scoreRule :: Query -> Rule -> Double
scoreRule q r =
  fromIntegral (keywordMatches r) * 1.0
  + (if categoryMatch q r then 10.0 else 0.0)
  + fromIntegral (tagMatches q r) * 2.0
  + (if titleMatch q r then 5.0 else 0.0)
```

## 6. Git Integration for Version History

### Requirements
- FR-029: Track rule changes over time
- Clarification: Version number + changelog in metadata, git for full history
- Must not require git commands at runtime

### Options Evaluated

**Option A: No git integration, rely only on YAML changelog**
- **Pros**: Simplest, no runtime dependencies
- **Cons**: Cannot show full diffs, limited historical queries

**Option B: Shell out to `git` commands**
- **Pros**: Full git power available
- **Cons**:
  - Brittle (requires git in PATH)
  - Hard to test
  - Not cross-platform friendly

**Option C: `gitlib` or `git` Haskell library**
- **Pros**: Type-safe git operations
- **Cons**: Heavy dependencies, complex API

### Decision: **No runtime git integration (Option A)**

**Rationale**:
- **Version number + changelog** in YAML metadata sufficient for MVP
- Users can use standard `git log`, `git diff` for detailed history
- Avoids shell dependency complexity
- Follows UNIX philosophy: do one thing well (we query rules, git tracks versions)
- Future enhancement: Optional `--git-log` flag can shell out if needed

**File-based Version Tracking**:
```yaml
# In rule frontmatter
rules:
  - id: COMBAT-1.2
    version: 2.1
    changelog:
      - version: 2.1
        date: 2025-11-15
        changes: "Reduced critical multiplier from 3x to 2x for balance"
      - version: 2.0
        date: 2025-10-01
        changes: "Initial combat rule definition"
```

## 7. Data Structures & Indexing

### Requirements
- Fast lookup by rule ID (FR-002)
- Filter by category, system, tags
- Support 1000+ rules efficiently

### Decision: `containers` library

**Data Structures**:
```haskell
-- RpgRuleset/Core/Types.hs
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

-- Primary index: O(log n) lookup
type RuleIndex = Map RuleId Rule

-- Secondary indexes for filtering
data QueryIndex = QueryIndex
  { rulesByCategory :: Map Category (Set RuleId)
  , rulesBySystem :: Map SystemId (Set RuleId)
  , rulesByTag :: Map Tag (Set RuleId)
  , allRules :: RuleIndex
  }
```

**Rationale**:
- `Map` provides O(log n) lookup, sufficient for 1000s of rules
- No need for database (constitutional file-based storage)
- In-memory indexing fast enough for success criteria
- `Set` for efficient intersection when filtering

## 8. CLI Framework

### Decision: `optparse-applicative`

**Rationale**:
- Already used in project (see CLAUDE.md: "optparse-applicative (already in project)")
- Best-in-class CLI parsing for Haskell
- Automatic help generation
- Type-safe argument handling
- Subcommand support for `query`, `validate`, `export`, etc.

**Example CLI Structure**:
```haskell
-- CLI/Commands.hs
data Command
  = Query QueryOpts
  | Validate ValidateOpts
  | Export ExportOpts
  | Import ImportOpts

queryOpts :: Parser QueryOpts
queryOpts = QueryOpts
  <$> strArgument (metavar "KEYWORDS")
  <*> optional (strOption (long "category"))
  <*> optional (strOption (long "system"))
```

## Technology Stack Summary

| Component | Technology | Rationale |
|-----------|-----------|-----------|
| **YAML Parsing** | `yaml` (Data.Yaml) | Aeson integration, excellent errors |
| **Markdown Parsing** | `cmark` (MVP) → `commonmark-hs` (future) | Fast, lightweight, extensible path |
| **Formula/Condition Parsing** | Text storage (MVP) → `megaparsec` (Phase 2c) | Phased complexity |
| **File System** | `directory` + `filepath` | Zero extra deps, simple |
| **Query Ranking** | Hybrid scoring (keyword + category + tag) | Right complexity for scale |
| **Version History** | YAML metadata only (git external) | UNIX philosophy |
| **Data Structures** | `containers` (Map, Set) | O(log n) sufficient |
| **CLI** | `optparse-applicative` | Already in project |
| **Testing** | Hspec + QuickCheck | Constitutional requirement |
| **Build** | Bazel | Constitutional requirement |

## Dependency Additions Required

Add to `MODULE.bazel`:
```python
stack.package(name = "yaml")           # YAML frontmatter
stack.package(name = "cmark-gfm")      # Markdown parsing
stack.package(name = "hspec")          # Testing (if not present)
```

Already available (no additions needed):
- `aeson` (JSON, YAML integration)
- `containers` (Map, Set)
- `text` (text processing)
- `directory`, `filepath` (file system)
- `optparse-applicative` (CLI)

## Performance Validation

**Query Performance (SC-001: < 2 seconds)**:
- Index build: O(n) where n = number of rules
  - For 1000 rules: ~100ms (file I/O dominates)
- Query execution: O(n) scan with scoring
  - For 1000 rules: ~10ms (in-memory scoring)
- **Total**: Well under 2 seconds ✅

**Validation Performance (SC-009: < 2 minutes)**:
- Duplicate ID check: O(n) with Set
- Tag similarity: O(t²) where t = unique tags (~100) = ~10ms
- Cross-reference validation: O(r) where r = references per rule
- Formula validation (Phase 2c): O(f) where f = formulas
- **Total for 1000 rules**: ~5-10 seconds ✅

## Risks & Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| `cmark` insufficient for tables | Medium | Low | Migration path to `commonmark-hs` documented |
| Formula DSL complexity underestimated | Medium | Medium | Phased approach (string storage → parsing) |
| Query performance degrades >5000 rules | Low | Medium | Can add caching, pre-computed indexes |
| YAML parsing errors confuse users | Medium | High | Comprehensive error messages, validation examples |

## Next Steps

1. ✅ Technology choices validated and documented
2. ⏭️ Proceed to Phase 1: Data Model Design (`data-model.md`)
3. ⏭️ Define contracts (`contracts/rule-schema.yaml`, `cli-interface.md`, `query-api.md`)
4. ⏭️ Create quickstart guide (`quickstart.md`)

## References

- [yaml documentation](https://hackage.haskell.org/package/yaml)
- [cmark-gfm documentation](https://hackage.haskell.org/package/cmark-gfm)
- [megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)
- [optparse-applicative guide](https://hackage.haskell.org/package/optparse-applicative)

**Research complete. All technology decisions validated against constitutional requirements and success criteria.**
