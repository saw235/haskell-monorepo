# Feature Specification: Tabletop RPG Ruleset Documentation & Query System

**Feature Branch**: `001-dnd-ruleset-query`
**Created**: 2025-11-15
**Status**: Draft
**Input**: User description: "Want to create methodology for documenting or listing down the rulesets of dnd, to allow for easy querying. I am mainly interested in the rulesets governing character creation, world building and interactions."

## Clarifications

### Session 2025-11-15

- Q: How should rule content be stored - citations only, paraphrased text, or verbatim text? → A: Store full verbatim rule text
- Q: What is the initial data volume scope for rule documentation? → A: Start with 1-2 core rulebooks per system initially
- Q: How should query results be ranked? → A: Relevance-based ranking using keyword matching and category alignment
- Q: Is this for documenting existing published systems or creating your own? → A: Creating own custom RPG system (may adapt/modify rules from existing systems)
- Q: What interface should be used for rule authoring? → A: Structured data files (JSON/YAML/Markdown) that creator edits directly
- Q: How should rule files be organized in the file system? → A: Directory per category/subcategory, with related rules grouped in files using unique IDs
- Q: What structure should the tagging/keyword system use? → A: Free-form tags with suggested conventions (e.g., mechanics vs. lore tags)
- Q: What should the markdown file structure be? → A: Markdown with YAML frontmatter containing file metadata and array of rule metadata, with rule text in markdown body sections
- Q: How should new rule consistency be validated? → A: Validation checklist workflow - system guides creator through consistency checks when adding rules
- Q: Should validation checklists be category-specific? → A: Category-specific checklists with common base checks (e.g., Character Creation validates stat balance, World Building validates location consistency)
- Q: How should system inheritance/extension work? → A: System variants with base inheritance, with support for cross-system references (B + possibility of D)
- Q: How should worlds/campaigns relate to rulesets? → A: Hybrid: Independent systems + optional base/variant relationships (some worlds share base, others are independent)
- Q: How should character/content portability between worlds work? → A: Export/import with validation warnings (automatic rule mapping planned for future)
- Q: How should rule versioning work? → A: Hybrid - version number + summary changelog in metadata, git for full history
- Q: How should conditional/calculated rules be structured? → A: Structured metadata for conditions + formulas (machine-readable)
- Q: How should access control work (GM-only rules)? → A: Visibility levels in rule metadata with query-time filtering
- Q: How should tables and formulas be formatted? → A: Markdown tables + structured formula metadata (formulas in YAML, rendered in markdown)

### Session 2025-11-16

- Q: What format should rule IDs use? → A: Uppercase abbreviations with zero-padded numbers (e.g., CMBT-999, MAGK-001, CHAR-001). Tags remain lowercase (e.g., mechanics, combat, attack).
- Q: What are the exact validation rules for rule ID format? → A: Flexible prefix (2-6 uppercase letters), hyphen, 3 zero-padded digits (e.g., CH-001, CHAR-001, CHRGEN-001). Allows shorter/longer abbreviations as needed.
- Q: What are the scalability limits for the system? → A: Medium scale - max 1000 rules per system, max 10 systems, up to 5 concurrent users. Balanced for typical usage with multiple campaigns.
- Q: What should happen when validation detects a duplicate rule ID? → A: Block creation - validation fails with error, rule cannot be saved until ID is changed to a unique value.
- Q: Should rule ID prefixes follow standardized conventions or be free-form? → A: Suggested conventions - system provides recommended prefixes per category (e.g., CHAR for character-creation, WRLD for world-building) but allows free-form. Validation warns if prefix doesn't follow convention but doesn't block.
- Q: How do users identify themselves as GM or Player for visibility filtering? → A: CLI flag/option - single query command with `--role` flag (e.g., `--role gm` or `--role player`). Defaults to player visibility. Simple trust-based system appropriate for small team use.
- Q: When a variant system extends a base system, how should rule ID conflicts be handled if both define the same rule ID? → A: Override behavior - Variant rule completely replaces base rule in variant context. Within-system references use unqualified IDs (e.g., CHAR-001). Cross-system references use system-id/RULE-ID format (e.g., base-system/CHAR-001, variant-darklands/CHAR-001) to explicitly specify which version.
- Q: How should GM-only visibility terminology be standardized across YAML, display, and code? → A: YAML and user-facing use lowercase-hyphenated (`visibility: gm-only`). Display headers and messages use capitalized "GM-Only". Haskell code uses `data Visibility = Public | GMOnly` with parser mapping "gm-only" → GMOnly constructor.
- Q: Should System entity include inheritance types (Base/Variant) in MVP Foundation phase, or defer to Multi-World phase? → A: Include types in Foundation, defer resolution logic. Foundation implements complete System data type with `SystemType = BaseSystem | VariantSystem` for type safety. MVP treats all systems as independent (no inheritance resolution). Inheritance resolution logic (merging base + variant rules) deferred to Phase 2b (Multi-World).
- Q: Should vague non-functional requirements (e.g., "intuitive", "readable format") be converted to measurable acceptance criteria? → A: Keep as qualitative guidelines. NFRs like usability and readability rely on developer judgment during implementation. Concrete metrics exist where critical (e.g., query < 2s, scalability limits in SC-001 to SC-016).
- Q: What should happen when a public rule references a GM-only rule (visibility conflict)? → A: Show redacted reference. Player queries display the public rule normally but show GM-only references as "[Hidden Rule]" or "[GM-Only Reference]" without revealing rule ID or details. GM queries show full references. Preserves rule structure while maintaining mystery.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Query Character Creation Rules (Priority: P1)

A game master or player needs to quickly look up specific rules about character creation, such as attribute generation methods, ancestry/lineage selection requirements, character class prerequisites, or background options. They want to find the exact rule without manually searching through multiple rulebooks.

**Why this priority**: Character creation is fundamental to starting any tabletop RPG game. This is the most common query need and delivers immediate value for both new and experienced players.

**Independent Test**: Can be fully tested by querying "How do I generate character attributes?" and receiving all valid methods (standard array, point allocation, random generation) with their specific rules, delivering immediate practical value for character creation.

**Acceptance Scenarios**:

1. **Given** a user wants to create a spellcaster character, **When** they query "What are the requirements for spellcaster class?", **Then** the system returns attribute requirements, proficiencies, and starting equipment
2. **Given** a user is generating character attributes, **When** they query "character attribute generation methods", **Then** the system lists all valid methods with complete rules for each
3. **Given** a user wants to understand ancestry options, **When** they query "ancestral traits", **Then** the system returns attribute modifiers, languages, special abilities, and variants

---

### User Story 2 - Query World Building Rules (Priority: P2)

A game master is designing a campaign setting and needs to reference rules about creating locations, factions, magic systems, or environmental features. They want to find guidelines and constraints that govern world consistency.

**Why this priority**: World building is essential for game masters but happens less frequently than character creation. It provides significant value for campaign preparation.

**Independent Test**: Can be tested by querying "What are the rules for creating a settlement?" and receiving guidelines on population sizes, government types, available services, and typical power levels, delivering standalone value for campaign design.

**Acceptance Scenarios**:

1. **Given** a GM is creating a new settlement, **When** they query "settlement size categories", **Then** the system returns population ranges, typical services, and characteristics for different settlement types
2. **Given** a GM wants to design magical item distribution, **When** they query "magical item availability by settlement", **Then** the system returns rarity tables and acquisition rules for different settlement sizes
3. **Given** a GM is designing environmental challenges, **When** they query "extreme environment effects", **Then** the system returns mechanical effects for different environmental conditions

---

### User Story 3 - Query Interaction & Social Rules (Priority: P2)

Players and game masters need to reference rules governing character interactions including skill checks, social encounters, influence mechanics, and NPC reaction systems.

**Why this priority**: Social interactions are frequent in gameplay but typically need less detailed rule lookup than character creation. Rules tend to be more straightforward but still require quick reference.

**Independent Test**: Can be tested by querying "How does persuasion work?" and receiving the skill check mechanics, difficulty guidelines, and modifying circumstances, delivering standalone value during social encounters.

**Acceptance Scenarios**:

1. **Given** a player wants to persuade an NPC, **When** they query "persuasion check rules", **Then** the system returns the attribute used, how to calculate modifiers, and sample difficulties for different tasks
2. **Given** a GM needs to determine NPC reactions, **When** they query "NPC attitude system", **Then** the system returns attitude categories and how to shift them
3. **Given** players are engaging in social conflict, **When** they query "opposed social checks", **Then** the system returns contested check rules and consequences of success/failure

---

### User Story 4 - Cross-Reference Related Rules (Priority: P3)

Users need to find rules that are related or interact with each other, such as how multiclassing affects abilities, or how ancestral features interact with class abilities.

**Why this priority**: This represents advanced usage and helps experienced users understand complex rule interactions. While valuable, it builds on the foundation of basic queries.

**Independent Test**: Can be tested by querying "How does multiclassing affect spellcasting?" and receiving ability calculation tables and rules for multiple spellcasting classes, delivering value for complex character builds.

**Acceptance Scenarios**:

1. **Given** a player is multiclassing spellcasters, **When** they query "multiclass spellcasting rules", **Then** the system returns ability calculation method and limitations
2. **Given** a user wants to understand feature stacking, **When** they query "Do defensive abilities stack?", **Then** the system returns the specific rule about combining or not combining defensive calculation methods
3. **Given** a player has ancestral abilities and class abilities, **When** they query "ancestral ability interaction with class", **Then** the system clarifies how attributes and ability lists interact

---

### User Story 5 - Validate New Rules for Consistency (Priority: P1)

The system creator is adding a new rule to their custom RPG system and needs to ensure it doesn't contradict or overlap with existing rules. They want guided assistance to check for potential conflicts.

**Why this priority**: Rule consistency is critical for a playable RPG system. Contradictions confuse players and undermine the game. This is a core authoring workflow that must work from day one.

**Independent Test**: Can be fully tested by adding a new rule about attribute modifiers and being guided through checks that surface the existing CHAR-002 rule, ensuring the creator reviews it for consistency.

**Acceptance Scenarios**:

1. **Given** creator is adding a new Character Creation rule, **When** they invoke the validation workflow, **Then** the system presents common base checks (duplicate ID, tag similarity) PLUS category-specific checks (stat balance, attribute modifier ranges)
2. **Given** creator is modifying a World Building rule, **When** running validation, **Then** the system checks for location consistency and displays rules that reference the modified rule
3. **Given** creator is removing an Interactions rule, **When** running validation, **Then** the system identifies dependent rules that reference the rule being removed and warns about potential breaks
4. **Given** a new rule has tags matching existing rules, **When** running validation, **Then** the system displays potentially related rules for creator review
5. **Given** creator confirms no contradictions found, **When** completing the validation checklist, **Then** the new rule is marked as validated and ready for use

---

### User Story 6 - Create and Add New Rules via CLI (Priority: P1)

The system creator wants to add a new rule to their custom RPG system using the command-line interface. They can use either an interactive guided workflow or non-interactive mode with command-line options to create the rule file with proper structure, assign a unique ID following conventions, set appropriate metadata (category, tags, visibility), and write the rule content before running validation.

**Why this priority**: Rule creation is the fundamental authoring workflow - without it, there's nothing to query or validate. This is a core capability required from day one alongside querying and validation. CLI integration ensures consistency with the existing query and validate commands.

**Independent Test**: Can be fully tested by running `rpg-ruleset-query add --system my-rpg --interactive` and following the guided prompts to create a Character Creation rule with ID CHAR-003, then verifying the system accepts the new rule file and includes it in query results.

**Acceptance Scenarios**:

1. **Given** creator wants to add a new rule, **When** they run `rpg-ruleset-query add --system my-rpg --interactive`, **Then** the CLI guides them through category selection, ID assignment, metadata entry, and content creation
2. **Given** creator is adding a Character Creation rule via CLI, **When** they reach the ID assignment step, **Then** the CLI suggests the CHAR prefix convention, shows existing IDs in the category, and suggests the next available ID
3. **Given** creator provides an ID that already exists in the system, **When** attempting to add the rule, **Then** the CLI rejects the duplicate ID with a clear error message identifying the conflicting rule
4. **Given** creator sets visibility to `gm-only` via `--visibility gm-only`, **When** the rule is created, **Then** the rule is hidden from player queries while remaining visible to GM queries
5. **Given** creator has a pre-written markdown file with rule content, **When** they run `rpg-ruleset-query add --system my-rpg --file rule.md`, **Then** the CLI parses the file and adds the rule with validation
6. **Given** creator uses non-interactive mode with all options, **When** they run `rpg-ruleset-query add --system my-rpg --category character-creation --id CHAR-003 --title "Rule Title" --tags tag1 --tags tag2 --file content.md`, **Then** the rule is created without prompts
7. **Given** creator has completed rule content and metadata, **When** the add command finishes, **Then** validation runs automatically unless `--skip-validation` is specified

---

### User Story 7 - Manage Multi-World System Variants (Priority: P2)

The system creator has a base RPG system and wants to create variant systems for different worlds/campaigns. Each world extends the base with custom rules while inheriting core mechanics. They also want to move content (characters, items) between compatible worlds.

**Why this priority**: Multi-world support is essential for long-term use but builds on single-system functionality. This enables reusability of base rules across multiple campaigns while allowing customization per world.

**Independent Test**: Can be fully tested by creating a base system with core rules, then creating two variant worlds that extend it with different custom rules, and verifying inheritance works correctly (variant inherits base unless overridden).

**Acceptance Scenarios**:

1. **Given** a base system exists with core rules, **When** creator creates a variant system extending the base, **Then** the variant inherits all base rules automatically
2. **Given** a variant system overrides a base rule, **When** querying that rule in the variant, **Then** the system returns the variant's version, not the base version
3. **Given** creator exports a character from World A, **When** importing to World B using a different system, **Then** the system validates compatibility and warns about rules that don't exist in World B
4. **Given** a rule in System A references a rule in System B, **When** validating the cross-system reference, **Then** the system checks the referenced rule exists and warns if not found

---

### User Story 8 - GM-Only Content Access Control (Priority: P2)

A game master is documenting rules that contain spoilers or secret information that should not be visible to players (e.g., hidden monster weaknesses, secret plot rules, GM guidance). They want to mark these rules as GM-only so they don't appear in player queries.

**Why this priority**: Essential for maintaining game mystery and preventing metagaming. GMs need to document everything without worrying about spoiling surprises.

**Independent Test**: Can be fully tested by creating a rule marked `visibility: gm-only`, then querying as a player (rule hidden) vs. querying as GM (rule visible).

**Acceptance Scenarios**:

1. **Given** a rule is marked `visibility: gm-only`, **When** a player queries for that rule by keyword, **Then** the rule does not appear in search results
2. **Given** a rule is marked `visibility: public`, **When** any user queries for that rule, **Then** the rule appears in search results for all users
3. **Given** a GM is browsing rules, **When** viewing a category, **Then** all rules (public and GM-only) are visible with clear indicators
4. **Given** a public rule references a GM-only rule, **When** a player views the referencing rule, **Then** the system displays the public rule normally but shows GM-only references as "[Hidden Rule]" without revealing rule ID or details. When a GM views the same rule, all references are shown normally.

---

### Edge Cases

- What happens when querying rules that vary between different RPG systems or editions?
- What happens when rules from different sourcebooks conflict within the same system?
- How does the system handle vague queries like "combat rules" (too broad)?
- What happens when querying optional rules vs core rules?
- How does the system handle errata and updated rules?
- What happens when multiple RPG systems have similarly named but different mechanics (e.g., "inspiration" in different systems)?
- What happens when validation workflow finds multiple potentially conflicting rules?
- How does the system handle false positives in rule conflict detection (rules that appear similar but don't actually conflict)?
- What happens when removing a rule that is referenced by many other rules (cascade effects)?
- How does validation handle rules that modify or extend other rules (inheritance/dependency chains)?
- What happens when a variant system overrides a base rule that other rules depend on (inheritance conflict)?
- What happens when exporting content that uses rules from multiple cross-referenced systems?
- How does the system handle circular system inheritance (System A extends B, B extends A)? (Note: Variant override behavior for same IDs is clarified in Session 2025-11-16; duplicates within a system are blocked by FR-018b)
- What happens when a conditional rule's condition references another rule that gets removed?
- How are formulas validated for correctness (e.g., `1d6 + nonexistent_modifier`)?
- How does the system handle version conflicts when importing content (source uses v1.0, target has v2.0)?
- What happens when a formula references attributes that don't exist in a variant system?

## Example Rule File Structure

Below is a complete example showing how all the specified features come together in a markdown file:

```markdown
---
category: character-creation/combat
system: my-fantasy-rpg
rules:
  - id: CMBT-012
    version: 2.1
    changelog:
      - version: 2.1
        date: 2025-11-15
        changes: "Reduced critical multiplier from 3x to 2x for balance"
      - version: 2.0
        date: 2025-10-01
        changes: "Added conditions for advantage/disadvantage"
      - version: 1.0
        date: 2025-09-01
        changes: "Initial melee attack rule"
    tags: [mechanics, combat, attack]
    visibility: public
    conditions:
      - "character.weapon_equipped == true"
      - "target.in_range == true"
    formulas:
      attack_roll: "1d20 + STR_modifier + proficiency_bonus"
      damage: "weapon_damage + STR_modifier"
      critical_damage: "weapon_damage * 2 + STR_modifier"
    related: [CMBT-013, CHAR-002]

  - id: CMBT-013
    version: 1.0
    changelog:
      - version: 1.0
        date: 2025-09-01
        changes: "Initial critical hit rule"
    tags: [mechanics, combat, critical]
    visibility: gm-only
    conditions:
      - "attack_roll >= 20"
    formulas:
      damage: "weapon_damage * 2"
    related: [CMBT-012]
---

# Combat Rules - Melee Attacks

## [CMBT-012] Melee Attack Resolution

When a character attempts a melee attack, follow these steps:

**Prerequisites:**
- Character must have a melee weapon equipped
- Target must be within weapon range

**Attack Roll Formula:** `1d20 + STR_modifier + proficiency_bonus`

**Damage Calculation:**

| Attack Type | Damage Formula | Notes |
|-------------|----------------|-------|
| Normal Hit  | `weapon_damage + STR_modifier` | On successful attack |
| Critical Hit | `weapon_damage * 2 + STR_modifier` | On natural 20 |

**Example:**
A character with STR modifier +3 and proficiency +2 attacks with a longsword (1d8 damage):
- Attack roll: 1d20 + 3 + 2 = 1d20 + 5
- Normal damage: 1d8 + 3
- Critical damage: 2d8 + 3

## [CMBT-013] Critical Hit Secrets *(GM-Only)*

This rule contains hidden information about critical hit vulnerabilities that players should not know...

---
```

**Key Features Demonstrated:**
- **Versioning**: Version 2.1 with changelog showing evolution
- **Conditional Rules**: Only applies when conditions are met
- **Formulas**: Machine-readable formulas in metadata + human-readable in text
- **Access Control**: COMBAT-1.3 is GM-only
- **Tables**: Damage calculation table in markdown
- **Cross-References**: Rules reference each other
- **Rich Metadata**: Tags, visibility, conditions all structured

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST organize rulesets into hierarchical categories: Character Creation, World Building, and Interactions
- **FR-002**: System MUST allow users to query rules using natural language questions
- **FR-003**: System MUST support keyword-based searching (e.g., "spellcaster", "attributes", "persuasion")
- **FR-004**: System MUST return full verbatim rule text with source citations (page numbers, sourcebook references)
- **FR-005**: System MUST distinguish between core rules and optional rules in query results
- **FR-006**: System MUST handle queries about rule interactions and cross-references
- **FR-007**: System MUST document rules in Markdown files with YAML frontmatter containing file metadata and per-rule metadata arrays
- **FR-008**: System MUST support filtering by RPG system and edition, using a system-agnostic architecture that can accommodate any tabletop RPG ruleset
- **FR-009**: System MUST provide examples with rule text when available
- **FR-010**: System MUST index rules by multiple access paths (category, keyword, entity type)
- **FR-011**: System MUST support browsing rules hierarchically (category → subcategory → specific rule)
- **FR-012**: System MUST handle ambiguous queries by presenting multiple relevant results ranked by keyword matching and category alignment
- **FR-013**: System MUST support free-form tagging of rules with keywords following suggested conventions (e.g., distinguishing mechanics tags from lore/flavor tags)
- **FR-014**: System MUST indicate when rules have been updated by errata
- **FR-016**: System MUST support rule authoring through Markdown files with YAML frontmatter that can be edited directly and version-controlled
- **FR-017**: System MUST organize rule files in a directory structure mirroring the category hierarchy, with related rules grouped in files and identified by unique IDs following the format: 2-6 uppercase letters, hyphen, 3 zero-padded digits (e.g., CHAR-001, WRLD-023, CHRGEN-042)
- **FR-018**: System MUST parse YAML frontmatter containing file-level metadata (category, system) and a rules array with per-rule metadata (id, tags, related, version)
- **FR-018a**: System MUST validate rule IDs match the required format: 2-6 uppercase letters, hyphen, 3 zero-padded digits (pattern: `^[A-Z]{2,6}-\d{3}$`)
- **FR-018b**: System MUST reject rules with duplicate IDs within a system - validation fails with error and rule cannot be saved until ID is changed to a unique value
- **FR-018c**: System SHOULD provide suggested rule ID prefix conventions per category (e.g., CHAR for character-creation, WRLD for world-building, INTR for interactions) and warn when prefixes don't follow conventions, but allow free-form prefixes without blocking validation
- **FR-019**: System MUST provide a validation checklist workflow to guide creator through consistency checks when adding, modifying, or removing rules (checking for contradictions, overlaps, and related rules)
- **FR-020**: System MUST support category-specific validation checklists with common base checks (e.g., Character Creation validates stat balance, World Building validates location consistency, Interactions validates mechanic coherence)
- **FR-021**: System MUST support system inheritance where a variant system can extend a base system, inheriting rules and allowing overrides
- **FR-022**: System MUST support cross-system rule references using the format `system-id/RULE-ID` (e.g., `base-system/CHAR-001`) to explicitly reference rules from other systems, while within-system references use unqualified IDs (e.g., `CHAR-001`)
- **FR-023**: System MUST allow both independent systems (standalone) and variant systems (extending a base) to coexist in the same repository
- **FR-024**: System MUST provide export/import functionality for content (characters, rules) between worlds with validation warnings for incompatible rules
- **FR-025**: System MUST support rule versioning with version numbers and summary changelogs in metadata (major changes only), leveraging git for full history
- **FR-026**: System MUST support conditional rules with structured condition metadata (e.g., applies only when character level >= 5)
- **FR-027**: System MUST support calculated rules with formula metadata (e.g., damage = 1d6 + STR_modifier) stored in machine-readable format
- **FR-028**: System MUST support visibility levels for rules (public, gm-only) with query-time filtering based on viewer role specified via CLI flag (e.g., `--role gm` or `--role player`, defaulting to player)
- **FR-029**: System MUST support markdown tables for structured data within rules (stat tables, price lists, probability charts)
- **FR-030**: System MUST render formulas from metadata into human-readable format in rule display
- **FR-031**: When a variant system defines a rule with the same ID as a rule in its base system, the variant's rule MUST completely override the base rule in the variant system context. Queries in the variant context return only the variant version. Queries in the base system context return the base version. Cross-system references using `base-system/RULE-ID` format explicitly specify which system's version to retrieve.
- **FR-032**: When a public rule references a GM-only rule, player queries MUST display the public rule normally but show GM-only references as "[Hidden Rule]" without revealing rule ID or details. GM queries MUST show all references normally with full details.
- **FR-033**: System MUST provide a CLI command (`rpg-ruleset-query add`) for rule addition that supports both interactive mode (guided prompts) and non-interactive mode (command-line options), guiding creators through: (1) selecting target category/directory, (2) assigning rule ID with format validation and convention suggestions, (3) setting required metadata (tags, visibility, version), (4) writing rule content, and (5) triggering validation
- **FR-034**: System MUST parse and index new rule files immediately upon creation, making them available for queries without manual reindexing
- **FR-035**: System MUST validate that all required YAML frontmatter fields are present when adding a rule: id, tags (array), version (number), and visibility (public or gm-only)
- **FR-036**: System MUST provide clear error messages when rule addition fails, identifying the specific validation failure (missing field, invalid ID format, duplicate ID, invalid visibility value)
- **FR-037**: System MUST support batch rule addition by parsing markdown files containing multiple rules in the same file (multiple entries in the rules array)
- **FR-038**: System MUST integrate rule addition with validation workflow - after a rule is added, system prompts creator to run validation checklist

### Key Entities

- **Rule**: A discrete rule statement with unique ID following format of 2-6 uppercase letters, hyphen, 3 zero-padded digits (e.g., CHAR-001, CMBT-042, CHRGEN-100), category, text content (markdown with tables), optional source reference, system/edition, free-form lowercase keywords/tags (e.g., mechanics, combat, attack), version number with summary changelog, optional conditions (when rule applies), optional formulas (calculations), visibility level (`public` or `gm-only` in YAML, displayed as "Public" or "GM-Only"), and related rules
- **Category**: Hierarchical classification including Character Creation (Attributes, Ancestries, Classes, Backgrounds, Equipment), World Building (Settlements, Magical Items, Environments, Factions, Cosmology), Interactions (Skill Checks, Social Encounters, Influence Mechanics, Status Effects)
- **Source**: Reference to official rulebook or document including title, page number, system/edition, and publication date
- **Query**: User search request with search terms, filters (category, system, source), and relevance-based result ranking determined by keyword match strength and category alignment
- **Cross-Reference**: Relationship between rules including type (modifies, conflicts with, requires, related to, extends) and explanation
- **Validation Checklist**: Category-specific validation workflow with common base checks (duplicate ID, tag overlap, reference integrity) and category-specific checks (stat balance for Character Creation, location consistency for World Building, mechanic coherence for Interactions)
- **System**: The specific RPG ruleset (e.g., different editions or variants) with version information, system type (`BaseSystem` or `VariantSystem` with base system reference for inheritance), and metadata. Note: MVP Foundation implements complete System data type but treats all systems as independent; inheritance resolution deferred to Multi-World phase.
- **World**: A specific game setting/campaign that uses a system (either base or variant), containing characters, locations, and campaigns using the ruleset
- **Content Package**: Exportable bundle of rules, characters, or other content that can be imported into different worlds with compatibility validation

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can find a specific character creation rule in under 30 seconds from query to relevant result
- **SC-002**: System returns relevant results for at least 90% of well-formed queries about documented rules
- **SC-003**: Users can successfully answer their rules question without consulting physical rulebooks in 80% of cases
- **SC-004**: Query results include source citations 100% of the time for traceability
- **SC-005**: Users can browse from category to specific rule in no more than 3 navigation steps
- **SC-006**: System accurately identifies rule conflicts or interactions when they exist
- **SC-007**: New users can successfully query and find basic rules (attributes, class features) on first attempt without training
- **SC-008**: System supports rules from multiple RPG systems without confusion or cross-contamination in search results
- **SC-009**: Creator can validate a new rule against existing rules in under 2 minutes using the validation checklist workflow
- **SC-010**: System correctly resolves rule inheritance (variant system inherits base system rules unless overridden)
- **SC-011**: Export/import validates cross-world content compatibility and warns about incompatible rules before import
- **SC-012**: Version changelog is visible and understandable, showing rule evolution over time
- **SC-013**: Conditional rules correctly evaluate their conditions and only apply when conditions are met
- **SC-014**: Formulas are validated for correctness (references to valid attributes/modifiers)
- **SC-015**: GM-only rules are completely hidden from player queries (0% visibility to players)
- **SC-016**: Tables and formulas render correctly in rule display (readable, properly formatted)
- **SC-017**: Creator can add a new rule with proper metadata and content in under 5 minutes using the guided workflow
- **SC-018**: System rejects 100% of rules with invalid ID format, duplicate IDs, or missing required fields with actionable error messages
- **SC-019**: Newly added rules are queryable immediately without requiring manual reindexing or system restart
- **SC-020**: 90% of rule addition errors are resolved on first attempt due to clear, specific error messages

## Assumptions *(mandatory)*

- Ruleset documentation will focus on tabletop RPG systems (not video game adaptations)
- Users (game masters and players) have basic familiarity with tabletop RPG terminology
- Primary use case is documenting custom/original RPG systems created by the system owner
- System may also be used to document adaptations or modifications of existing published rulesets
- Initial release will document core rules for the owner's custom RPG system
- English language content is the primary focus
- Rule updates are controlled by the system owner/creator
- The methodology should be extensible to support multiple custom or published RPG systems over time
- Systems can exist as independent rulesets OR as variants extending base systems
- Multiple worlds/campaigns can coexist, each using different systems or variants
- Content portability between worlds uses validation warnings initially; automatic rule mapping is a future enhancement
- MVP (Phase 2a) implements complete System data types including BaseSystem/VariantSystem distinction but treats all systems as independent; inheritance resolution logic is deferred to Phase 2b (Multi-World implementation)

## Scope *(mandatory)*

### In Scope

- Character creation rules (attributes, ancestries/races, classes, backgrounds, equipment, alignment/morality systems)
- World building rules (settlement creation, magical item distribution, environmental rules, faction systems, NPC guidelines, cosmology/planes)
- Interaction rules (skill/ability checks, social encounter mechanics, influence systems, status effects)
- Cross-referencing related rules and identifying conflicts
- Hierarchical browsing and keyword search capabilities
- Source citation and system/edition tracking
- Initial documentation of core rules for creator's custom RPG system
- Support for multiple custom or published RPG systems with clear separation
- Version control-friendly structured data format for rule content
- Validation checklist workflow for ensuring rule consistency during authoring (add, modify, remove operations)
- Category-specific validation checks (Character Creation: stat balance; World Building: location consistency; Interactions: mechanic coherence)
- System inheritance/variant support (base systems + variant extensions)
- Cross-system rule references (rules can reference rules from other systems)
- World/campaign management (multiple game instances using different systems)
- Content export/import with compatibility validation between worlds
- Rule versioning with summary changelogs (git-backed)
- Conditional rules (rules that apply only under specific conditions)
- Calculated rules with formulas (damage calculations, modifiers)
- Access control with visibility levels (GM-only vs. public rules)
- Rich content support (markdown tables, structured formulas)

### Out of Scope

- Combat rules and initiative systems (not included in "character creation, world building, and interactions" focus)
- Spell descriptions and spell mechanics (separate from the three focus areas)
- Monster statistics and encounter balancing
- Map creation and battle grid tools
- Dice rolling functionality
- Character sheet management
- Campaign planning or session notes
- Automated character validation or build optimization
- Real-time multiplayer features
- Digital marketplace or content distribution
- Homebrew/custom content support (deferred to future releases)
- Automatic rule mapping for cross-world content portability (planned for future; initial release uses validation warnings only)
- Visual rule dependency graphs
- Automated playtesting or balance analysis

## Dependencies *(mandatory)*

- Markdown files with YAML frontmatter for rule storage and retrieval
- Directory structure for organizing rules hierarchically by category with support for system inheritance (e.g., `base-system/`, `variant-system/extends/base-system/`)
- Search/query engine for natural language processing (keyword matching and category-based) with cross-system search capability
- System inheritance resolution mechanism to merge base system rules with variant overrides
- Cross-system reference validation to ensure referenced rules exist
- Clear system/edition metadata to prevent unintended cross-contamination between different RPG systems
- YAML parser to read frontmatter metadata (including conditions, formulas, changelog) and Markdown parser for rule content (including tables)
- File traversal capability to read directory hierarchies and resolve inheritance chains
- Export/import functionality for content packages with compatibility validation
- Formula parser and validator to check formula syntax and attribute references
- Condition evaluator to determine when conditional rules apply
- Access control filter to hide GM-only rules from player queries (trust-based via CLI `--role` flag, no authentication infrastructure required)
- Version control system (git) for full rule history beyond changelog summaries
- Markdown table renderer for displaying structured data
- Text editor for creator to author ruleset content in Markdown with YAML frontmatter

## Non-Functional Requirements *(optional)*

**Note**: The following are qualitative guidelines to inform implementation. Concrete metrics are provided in Success Criteria (SC-001 to SC-016) where measurability is critical.

### Usability

- Query interface should be intuitive for both new and experienced tabletop RPG players
- Results should be displayed in readable format with clear hierarchy
- Navigation should allow quick movement between related rules
- System should clearly indicate which RPG system/edition each rule belongs to

### Performance

- Query results should be returned in under 2 seconds for typical searches
- System should handle concurrent queries from multiple users without degradation

### Scalability

- System MUST support up to 1000 rules per system without performance degradation
- System MUST support up to 10 different systems in a single repository
- System MUST handle up to 5 concurrent users querying or validating rules simultaneously
- Query performance target (< 2 seconds) MUST be maintained at maximum scale (10 systems × 1000 rules = 10,000 total rules)

### Data Quality

- Rule content must be internally consistent within each custom system
- Version/revision tracking should maintain history of rule changes
- Cross-system rules should be clearly differentiated to prevent confusion
- Markdown files with YAML frontmatter must validate against defined schema
- YAML frontmatter must include required fields: file-level (category, system) and per-rule (id, tags, version)
- Tag conventions should be documented but not enforced (allowing flexibility in custom system development)

### Extensibility

- Rule documentation format should be flexible enough to accommodate different RPG systems
- New systems should be addable without restructuring existing content
- Category hierarchies should be customizable per system while maintaining consistent query interface
