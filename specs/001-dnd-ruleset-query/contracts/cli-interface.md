# CLI Interface Specification

**Feature**: RPG Ruleset Query System
**Version**: 1.0.0
**Related**: [data-model.md](../data-model.md), [rule-schema.yaml](./rule-schema.yaml)

## Overview

This document specifies the command-line interface for the `rpg-ruleset-query` application. All commands follow standard UNIX conventions with `-h/--help`, exit codes, and composable output formats.

## Application Name

```bash
rpg-ruleset-query
```

**Alias** (optional): `rpgq` (for convenience)

## Global Options

These options apply to all commands:

```
GLOBAL OPTIONS:
  -r, --ruleset-dir PATH    Path to rulesets directory (default: ./rulesets)
  -v, --verbose             Enable verbose logging
  -h, --help                Show help and exit
  --version                 Show version and exit
```

## Commands

### 1. query - Search for Rules

Search rules by keywords, category, system, or tags.

#### Synopsis

```bash
rpg-ruleset-query query [OPTIONS] [KEYWORDS...]
```

#### Options

```
QUERY OPTIONS:
  -c, --category CATEGORY      Filter by category (e.g., "character-creation")
  -s, --system SYSTEM          Filter by system ID (e.g., "my-fantasy-rpg")
  -t, --tag TAG                Filter by tag (can be specified multiple times)
  --visibility LEVEL           Filter by visibility (public | gm-only | all)
                               Default: public
  -l, --limit N                Limit results to N rules (default: 10)
  --offset N                   Skip first N results (default: 0)
  -f, --format FORMAT          Output format (text | json | markdown)
                               Default: text

DISPLAY OPTIONS:
  --show-content               Include full rule content in results
  --show-formulas              Include formulas in results
  --show-related               Show related rules
  --color / --no-color         Enable/disable colored output (default: auto)
```

#### Examples

**Basic keyword search:**
```bash
rpg-ruleset-query query critical hits
```
Output:
```
Found 3 rules (showing top 3):

[1] COMBAT-1.2 - Critical Hits (score: 15.2)
    System: my-fantasy-rpg
    Category: character-creation/combat
    Tags: mechanics, combat, attack, critical-hits
    File: rulesets/systems/my-fantasy-rpg/character-creation/attributes.md

    When you roll a natural 20 on an attack roll, you score a critical hit...

[2] COMBAT-3.5 - Critical Failure (score: 8.1)
    System: my-fantasy-rpg
    Category: character-creation/combat
    ...
```

**Filter by category:**
```bash
rpg-ruleset-query query combat --category character-creation/combat
```

**Filter by system:**
```bash
rpg-ruleset-query query magic --system my-fantasy-rpg
```

**Multiple tags:**
```bash
rpg-ruleset-query query --tag mechanics --tag combat
```

**GM-only rules:**
```bash
rpg-ruleset-query query secret --visibility gm-only
```

**JSON output for scripting:**
```bash
rpg-ruleset-query query critical --format json | jq '.results[0].rule.id'
```

**Show full content:**
```bash
rpg-ruleset-query query COMBAT-1.2 --show-content --show-formulas
```

#### Exit Codes

- `0` - Success (rules found)
- `1` - No rules found matching query
- `2` - Invalid arguments or options
- `3` - Ruleset directory not found or invalid

#### JSON Output Format

```json
{
  "query": {
    "keywords": ["critical", "hits"],
    "filters": {
      "category": null,
      "system": null,
      "tags": [],
      "visibility": "public"
    }
  },
  "results": [
    {
      "rule": {
        "id": "COMBAT-1.2",
        "category": "character-creation/combat",
        "system": "my-fantasy-rpg",
        "version": "2.1.0",
        "tags": ["mechanics", "combat", "attack", "critical-hits"],
        "visibility": "public",
        "title": "Critical Hits",
        "content": "When you roll a natural 20...",
        "formulas": {
          "attack_roll": "1d20 + STR_modifier + proficiency_bonus",
          "critical_damage": "weapon_damage * 2 + STR_modifier"
        },
        "conditions": ["character.weapon_equipped == true"],
        "related": ["COMBAT-1.3", "CHARACTER-0.2"],
        "sourceFile": "rulesets/systems/my-fantasy-rpg/character-creation/attributes.md"
      },
      "score": 15.2,
      "highlights": [
        {"field": "title", "snippet": "Critical Hits"},
        {"field": "content", "snippet": "...natural 20 on an attack roll, you score a critical hit..."}
      ]
    }
  ],
  "totalMatches": 3,
  "queryTime": 0.012
}
```

---

### 2. validate - Validate Rules

Run validation checklist on new or modified rules.

#### Synopsis

```bash
rpg-ruleset-query validate [OPTIONS] RULE_FILE
```

#### Options

```
VALIDATE OPTIONS:
  -c, --checklist CHECKLIST    Use specific checklist ID (default: auto-detect from category)
  --category CATEGORY          Specify category (overrides file frontmatter)
  -f, --format FORMAT          Output format (text | json)
                               Default: text
  --strict                     Treat warnings as errors
```

#### Examples

**Validate a rule file:**
```bash
rpg-ruleset-query validate rulesets/systems/my-rpg/character-creation/combat.md
```

Output:
```
Validating: combat.md
Category: character-creation/combat
Checklist: character-creation-checklist

Running checks...

✓ [PASS] Unique rule IDs (COMBAT-1.2, COMBAT-1.3)
✓ [PASS] Valid version format
✓ [PASS] All related rules exist
⚠ [WARNING] Tag similarity detected: "magic" vs "magick" (90% similar)
✓ [PASS] Formulas syntax valid
✓ [PASS] Conditions syntax valid
⚠ [WARNING] Missing changelog entry for version 2.0.0

Validation complete: 5 passed, 0 failed, 2 warnings
Status: PASS (warnings present)
```

**Strict mode (warnings = errors):**
```bash
rpg-ruleset-query validate --strict combat.md
```

Exit code: `1` (due to warnings)

**JSON output:**
```bash
rpg-ruleset-query validate combat.md --format json
```

```json
{
  "file": "combat.md",
  "category": "character-creation/combat",
  "checklist": "character-creation-checklist",
  "results": [
    {
      "checkItem": "Unique rule IDs",
      "status": "pass",
      "severity": "error",
      "message": "All rule IDs are unique: COMBAT-1.2, COMBAT-1.3"
    },
    {
      "checkItem": "Tag similarity",
      "status": "fail",
      "severity": "warning",
      "message": "Tag similarity detected: 'magic' vs 'magick' (90% similar)"
    }
  ],
  "overallStatus": "pass",
  "summary": {
    "passed": 5,
    "failed": 0,
    "warnings": 2
  }
}
```

#### Exit Codes

- `0` - Validation passed (no errors)
- `1` - Validation failed (errors found, or warnings in strict mode)
- `2` - Invalid arguments or file not found
- `3` - Checklist not found

---

### 3. list - List Systems and Categories

List available systems, categories, or rules.

#### Synopsis

```bash
rpg-ruleset-query list [systems | categories | rules] [OPTIONS]
```

#### Options

```
LIST OPTIONS:
  -s, --system SYSTEM          Filter by system ID
  -f, --format FORMAT          Output format (text | json | tree)
                               Default: text
```

#### Examples

**List all systems:**
```bash
rpg-ruleset-query list systems
```

Output:
```
Available Systems:

base-fantasy-rpg (Base System)
  Description: Core fantasy roleplaying rules
  Version: 1.2.0
  Rules: 487
  Path: rulesets/systems/base-fantasy-rpg/

my-fantasy-rpg-variant (Variant System)
  Base: base-fantasy-rpg
  Version: 1.0.0
  Rules: 52
  Path: rulesets/systems/my-fantasy-rpg-variant/

Total: 2 systems
```

**List categories:**
```bash
rpg-ruleset-query list categories
```

Output:
```
Categories:

character-creation/
  ├─ attributes
  ├─ combat
  └─ classes/
      ├─ fighter
      └─ wizard

world-building/
  ├─ geography
  └─ magic-system

interactions/
  ├─ combat
  └─ social
```

**List all rules in a system:**
```bash
rpg-ruleset-query list rules --system my-fantasy-rpg
```

**Tree format:**
```bash
rpg-ruleset-query list categories --format tree
```

---

### 4. export - Export Content Package

Export rules to a portable content package.

#### Synopsis

```bash
rpg-ruleset-query export [OPTIONS] OUTPUT_FILE
```

#### Options

```
EXPORT OPTIONS:
  -s, --system SYSTEM          System to export from (required)
  -r, --rules RULE_ID          Include specific rule (can be repeated)
  -c, --category CATEGORY      Include all rules in category
  --include-dependencies       Include all dependencies (related/cross-ref rules)
  --validate                   Validate package before export
  -f, --format FORMAT          Export format (json | yaml)
                               Default: json
```

#### Examples

**Export specific rules:**
```bash
rpg-ruleset-query export --system my-rpg --rules COMBAT-1.2 --rules COMBAT-1.3 package.json
```

**Export entire category:**
```bash
rpg-ruleset-query export --system my-rpg --category character-creation/combat combat-rules.json
```

**Include dependencies:**
```bash
rpg-ruleset-query export --system my-rpg --rules COMBAT-1.2 --include-dependencies package.json
```

Output:
```
Exporting from system: my-rpg
Selected rules: 1
Dependencies found: 2 (COMBAT-1.3, CHARACTER-0.2)
Total rules: 3

Validating package...
⚠ Warning: Cross-reference to external system: base-rpg/CORE-1.0

Package exported successfully: package.json
```

#### Exit Codes

- `0` - Export successful
- `1` - Export failed (validation errors if --validate)
- `2` - Invalid arguments or system not found

---

### 5. import - Import Content Package

Import rules from a content package.

#### Synopsis

```bash
rpg-ruleset-query import [OPTIONS] PACKAGE_FILE
```

#### Options

```
IMPORT OPTIONS:
  -s, --system SYSTEM          Target system to import into (required)
  --dry-run                    Validate without importing
  --skip-validation            Skip validation checks (dangerous)
  --resolve-conflicts MODE     How to handle conflicts (fail | skip | rename)
                               Default: fail
```

#### Examples

**Dry run import:**
```bash
rpg-ruleset-query import --system my-rpg --dry-run package.json
```

Output:
```
Importing into system: my-rpg
Package: combat-rules (version 1.0.0)
Rules to import: 3

Validation:
⚠ Warning: Rule ID conflict: COMBAT-1.2 already exists
⚠ Warning: Missing dependency: base-rpg/CORE-1.0 not found

Dry run complete. Use --resolve-conflicts to handle conflicts.
```

**Import with conflict resolution:**
```bash
rpg-ruleset-query import --system my-rpg --resolve-conflicts rename package.json
```

#### Exit Codes

- `0` - Import successful
- `1` - Import failed (validation errors or conflicts)
- `2` - Invalid arguments or package file not found

---

### 6. info - Show Rule Details

Display detailed information about a specific rule.

#### Synopsis

```bash
rpg-ruleset-query info [OPTIONS] RULE_ID
```

#### Options

```
INFO OPTIONS:
  -s, --system SYSTEM          System ID (required if rule ID not unique)
  --show-history               Show changelog history
  --show-related               Show related rules details
  -f, --format FORMAT          Output format (text | json | markdown)
                               Default: text
```

#### Examples

**Show rule details:**
```bash
rpg-ruleset-query info COMBAT-1.2 --system my-fantasy-rpg
```

Output:
```
Rule: COMBAT-1.2
Title: Critical Hits
System: my-fantasy-rpg
Category: character-creation/combat
Version: 2.1.0
Visibility: public
Tags: mechanics, combat, attack, critical-hits

Formulas:
  attack_roll: 1d20 + STR_modifier + proficiency_bonus
  critical_damage: weapon_damage * 2 + STR_modifier

Conditions:
  - character.weapon_equipped == true
  - character.level >= 1

Related Rules:
  - COMBAT-1.3 (Armor Class)
  - CHARACTER-0.2 (Proficiency Bonus)

Content:
  When you roll a natural 20 on an attack roll, you score a critical hit.

  Effect: Roll the weapon's damage dice twice and add modifiers once.

  Example: Longsword (1d8 + STR) critical = 2d8 + STR modifier

File: rulesets/systems/my-fantasy-rpg/character-creation/combat.md
```

**Show changelog:**
```bash
rpg-ruleset-query info COMBAT-1.2 --show-history
```

---

### 7. init - Initialize New System

Create directory structure and template files for a new system.

#### Synopsis

```bash
rpg-ruleset-query init [OPTIONS] SYSTEM_ID
```

#### Options

```
INIT OPTIONS:
  --name NAME                  Human-readable system name
  --description DESC           System description
  --base-system BASE_ID        Base system ID (for variants)
  --author AUTHOR              Author name
```

#### Examples

**Create base system:**
```bash
rpg-ruleset-query init my-new-rpg --name "My New RPG" --author "Game Designer"
```

Output:
```
Creating new base system: my-new-rpg
Path: rulesets/systems/my-new-rpg/

Created:
  ✓ rulesets/systems/my-new-rpg/system.yaml
  ✓ rulesets/systems/my-new-rpg/character-creation/
  ✓ rulesets/systems/my-new-rpg/world-building/
  ✓ rulesets/systems/my-new-rpg/interactions/
  ✓ rulesets/systems/my-new-rpg/README.md

System initialized successfully!
Next steps:
  1. Edit system.yaml to add metadata
  2. Create rule files in category directories
  3. Run 'rpg-ruleset-query validate' before committing
```

**Create variant system:**
```bash
rpg-ruleset-query init my-variant --base-system base-fantasy-rpg --name "My Variant"
```

---

### 8. add - Add New Rule

Create a new rule with guided workflow for metadata and validation.

#### Synopsis

```bash
rpg-ruleset-query add [OPTIONS]
```

#### Options

```
ADD OPTIONS:
  -s, --system SYSTEM          Target system ID (required)
  -c, --category CATEGORY      Category path (e.g., "character-creation/combat")
  --id RULE_ID                 Rule ID (if not provided, will be suggested)
  --title TITLE                Rule title
  --visibility LEVEL           Visibility (public | gm-only)
                               Default: public
  --tags TAG                   Add tag (can be repeated)
  -f, --file FILE              Markdown file with rule content
  --interactive                Interactive mode (prompts for all fields)
  --skip-validation            Skip validation after adding (not recommended)
```

#### Examples

**Interactive mode (guided workflow):**
```bash
rpg-ruleset-query add --system my-rpg --interactive
```

Output:
```
Adding new rule to system: my-rpg

[1/6] Category
Available categories:
  1. character-creation
  2. character-creation/combat
  3. character-creation/classes
  4. world-building
  5. interactions
Select category (or enter new path): 2

[2/6] Rule ID
Suggested prefix for 'character-creation/combat': CMBT
Existing IDs in category: CMBT-001, CMBT-002, CMBT-003
Suggested ID: CMBT-004
Enter rule ID [CMBT-004]: CMBT-004

[3/6] Title
Enter rule title: Flanking Bonus

[4/6] Visibility
  1. public (default)
  2. gm-only
Select visibility [1]: 1

[5/6] Tags
Enter tags (comma-separated): mechanics, combat, positioning
Suggested tags based on category: combat, attack
Final tags: mechanics, combat, positioning, attack

[6/6] Content
Enter rule content (end with Ctrl+D or empty line):
> When two allies are on opposite sides of an enemy, they gain a +2 bonus
> to attack rolls against that enemy.
>
> This bonus does not stack with other flanking bonuses.
> [Ctrl+D]

Creating rule file...
  File: rulesets/systems/my-rpg/character-creation/combat/flanking.md

Running validation...
✓ [PASS] Valid rule ID format (CMBT-004)
✓ [PASS] Unique rule ID
✓ [PASS] Required fields present
⚠ [WARNING] Similar rule found: CMBT-002 (Positioning Advantage) - 72% tag overlap
  Review suggested. Continue? [Y/n]: y

Rule added successfully!
  ID: CMBT-004
  File: rulesets/systems/my-rpg/character-creation/combat/flanking.md

Next steps:
  - Review related rule CMBT-002 for consistency
  - Run 'rpg-ruleset-query validate --strict' before committing
```

**Non-interactive mode with all options:**
```bash
rpg-ruleset-query add \
  --system my-rpg \
  --category character-creation/combat \
  --id CMBT-005 \
  --title "Opportunity Attacks" \
  --visibility public \
  --tags mechanics --tags combat --tags reaction \
  --file opportunity-attack-content.md
```

**Add from existing markdown file:**
```bash
rpg-ruleset-query add --system my-rpg --file new-rule.md
```

Where `new-rule.md` contains YAML frontmatter:
```markdown
---
category: character-creation/combat
rules:
  - id: CMBT-006
    tags: [mechanics, combat]
    visibility: public
    version: 1.0
---

# Movement in Combat

When a character moves during combat...
```

#### Exit Codes

- `0` - Rule added successfully
- `1` - Validation failed (invalid ID, duplicate, missing fields)
- `2` - Invalid arguments or system not found
- `3` - File not found or parse error

#### JSON Output Format

```bash
rpg-ruleset-query add --system my-rpg --file rule.md --format json
```

```json
{
  "status": "success",
  "rule": {
    "id": "CMBT-004",
    "title": "Flanking Bonus",
    "category": "character-creation/combat",
    "system": "my-rpg",
    "visibility": "public",
    "tags": ["mechanics", "combat", "positioning"],
    "version": "1.0",
    "file": "rulesets/systems/my-rpg/character-creation/combat/flanking.md"
  },
  "validation": {
    "status": "pass",
    "warnings": [
      {
        "type": "similar_rule",
        "message": "Similar rule found: CMBT-002 (72% tag overlap)",
        "relatedRuleId": "CMBT-002"
      }
    ]
  }
}
```

---

## Configuration File

Optional configuration file: `.rpg-ruleset-query.yaml` in home or project directory.

```yaml
# Default ruleset directory
rulesetDir: ./rulesets

# Default query options
query:
  limit: 10
  format: text
  visibility: public
  showColor: true

# Validation settings
validation:
  strict: false

# Export settings
export:
  format: json
  includeDependencies: true
```

---

## Environment Variables

- `RPGQ_RULESET_DIR` - Override default ruleset directory
- `RPGQ_VERBOSE` - Enable verbose logging (1 = enabled)
- `NO_COLOR` - Disable colored output

---

## Exit Codes Summary

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Operation failed (validation errors, no results, etc.) |
| 2 | Invalid arguments or options |
| 3 | Resource not found (directory, file, system, etc.) |

---

## Output Formats

All commands support multiple output formats for composability:

### Text (default)
Human-readable, colored output for terminal use.

### JSON
Machine-readable for scripting and integration.

### Markdown
For documentation and reports.

### Tree (list command only)
ASCII tree view for directory structures.

---

## Future Extensions

- `rpg-ruleset-query serve` - Start web interface
- `rpg-ruleset-query diff RULE_ID` - Show rule changes via git
- `rpg-ruleset-query check` - Full system consistency check
- `rpg-ruleset-query migrate` - Migrate rules between versions

---

## References

- [Data Model](../data-model.md) - Entity definitions
- [Rule Schema](./rule-schema.yaml) - YAML frontmatter schema
- [Query API](./query-api.md) - Query engine interface
