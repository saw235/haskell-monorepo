# Quickstart Guide: RPG Ruleset Query System

**Feature**: [spec.md](./spec.md) | **Plan**: [plan.md](./plan.md)
**Version**: 1.0.0
**Target**: Rule authors, game designers, system developers

## Overview

This guide helps you get started with the RPG Ruleset Documentation & Query System. You'll learn how to:

1. Create a new ruleset system
2. Write your first rules
3. Query and search rules
4. Validate rules for consistency
5. Export/import content between systems

**Prerequisites**: None! The system is file-based and uses simple Markdown + YAML.

## Table of Contents

- [Quick Start (5 minutes)](#quick-start-5-minutes)
- [Tutorial: Creating Your First System](#tutorial-creating-your-first-system)
- [Writing Rules](#writing-rules)
- [Querying Rules](#querying-rules)
- [Validation Workflow](#validation-workflow)
- [Multi-World Setup](#multi-world-setup)
- [Advanced Features](#advanced-features)

---

## Quick Start (5 minutes)

### 1. Install the CLI (when available)

```bash
# Via Bazel
bazel run //haskell/app/rpg-ruleset-query

# Or build and install
bazel build //haskell/app/rpg-ruleset-query
cp bazel-bin/haskell/app/rpg-ruleset-query/rpg-ruleset-query ~/bin/
```

### 2. Initialize Your First System

```bash
rpg-ruleset-query init my-fantasy-rpg \
  --name "My Fantasy RPG" \
  --author "Your Name"
```

This creates:
```
rulesets/
└── systems/
    └── my-fantasy-rpg/
        ├── system.yaml
        ├── character-creation/
        ├── world-building/
        ├── interactions/
        └── README.md
```

### 3. Write Your First Rule

Create `rulesets/systems/my-fantasy-rpg/character-creation/attributes.md`:

```markdown
---
category: character-creation
system: my-fantasy-rpg
rules:
  - id: ATTR-1.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Initial attribute rule"
    tags: [core-mechanics, attributes]
    visibility: public
---

## Attributes

### [ATTR-1.0] Core Attributes

Every character has six core attributes:

- **Strength (STR)**: Physical power and athleticism
- **Dexterity (DEX)**: Agility and reflexes
- **Constitution (CON)**: Endurance and health
- **Intelligence (INT)**: Reasoning and memory
- **Wisdom (WIS)**: Awareness and insight
- **Charisma (CHA)**: Force of personality

Attribute scores range from 1 to 20, with 10 being average.
```

### 4. Query Your Rules

```bash
# Search for all rules
rpg-ruleset-query query

# Search for specific keywords
rpg-ruleset-query query attributes

# Filter by category
rpg-ruleset-query query --category character-creation
```

### 5. Validate Your Rules

```bash
rpg-ruleset-query validate rulesets/systems/my-fantasy-rpg/character-creation/attributes.md
```

**Congratulations!** You've created your first RPG ruleset.

---

## Tutorial: Creating Your First System

Let's build a simple fantasy RPG system from scratch.

### Step 1: System Setup

```bash
rpg-ruleset-query init tutorial-rpg \
  --name "Tutorial Fantasy RPG" \
  --description "A beginner-friendly fantasy system" \
  --author "Tutorial Author"
```

### Step 2: Define Core Mechanics

Create `character-creation/core-mechanics.md`:

```markdown
---
category: character-creation
system: tutorial-rpg
rules:
  - id: CORE-1.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Initial core mechanics"
    tags: [core-mechanics, dice-rolls]
    visibility: public
    formulas:
      skill_check: "1d20 + attribute_modifier + skill_bonus"

  - id: CORE-1.1
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Difficulty class definitions"
    tags: [core-mechanics, difficulty]
    visibility: public
    related: [CORE-1.0]
---

## Core Mechanics

### [CORE-1.0] Skill Checks

When attempting a task with uncertain outcome, roll a **skill check**:

**Formula**: `1d20 + attribute_modifier + skill_bonus`

Compare the result to the Difficulty Class (DC):
- **Success**: Result ≥ DC
- **Failure**: Result < DC

---

### [CORE-1.1] Difficulty Classes

Use these standard DCs:

| Difficulty | DC | Example |
|------------|----|---------|
| Easy | 10 | Climb a ladder |
| Medium | 15 | Pick a simple lock |
| Hard | 20 | Decipher ancient text |
| Very Hard | 25 | Leap across a chasm |
```

### Step 3: Add Combat Rules

Create `character-creation/combat.md`:

```markdown
---
category: character-creation/combat
system: tutorial-rpg
rules:
  - id: COMBAT-1.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Initial combat rules"
    tags: [combat, attack]
    visibility: public
    related: [CORE-1.0]
    formulas:
      attack_roll: "1d20 + STR_modifier + proficiency_bonus"
      damage: "weapon_damage + STR_modifier"

  - id: COMBAT-1.1
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Critical hit rules"
    tags: [combat, critical-hits]
    visibility: public
    related: [COMBAT-1.0]
    conditions:
      - "attack_roll == 20"
    formulas:
      critical_damage: "weapon_damage * 2 + STR_modifier"
---

## Combat

### [COMBAT-1.0] Attacking

To attack a target:

1. Roll attack: `1d20 + STR_modifier + proficiency_bonus`
2. Compare to target's Armor Class (AC)
3. If attack ≥ AC, roll damage: `weapon_damage + STR_modifier`

**Example**: Fighter with +3 STR, +2 proficiency, longsword (1d8)
- Attack roll: 1d20 + 5
- Damage: 1d8 + 3

---

### [COMBAT-1.1] Critical Hits

When you roll a natural 20 on an attack roll:

**Effect**: Double the weapon damage dice

**Formula**: `weapon_damage * 2 + STR_modifier`

**Example**: Longsword critical = 2d8 + 3
```

### Step 4: Add World Building Rules

Create `world-building/magic.md`:

```markdown
---
category: world-building
system: tutorial-rpg
rules:
  - id: MAGIC-1.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Magic system overview"
    tags: [magic, worldbuilding]
    visibility: public
---

## Magic System

### [MAGIC-1.0] The Weave

Magic flows through an invisible energy field called **The Weave**.

**Sources of Magic**:
- **Arcane**: Study and intellect (wizards, artificers)
- **Divine**: Faith and devotion (clerics, paladins)
- **Primal**: Nature and instinct (druids, rangers)

**Magic Restrictions**:
- All magic requires line of sight
- Magic cannot create permanent matter
- Resurrection requires rare components
```

### Step 5: Validate Your System

```bash
rpg-ruleset-query validate rulesets/systems/tutorial-rpg/character-creation/core-mechanics.md
rpg-ruleset-query validate rulesets/systems/tutorial-rpg/character-creation/combat.md
rpg-ruleset-query validate rulesets/systems/tutorial-rpg/world-building/magic.md
```

### Step 6: Query Your Rules

```bash
# Find all combat rules
rpg-ruleset-query query combat

# Find rules with formulas
rpg-ruleset-query query --show-formulas

# Find magic-related rules
rpg-ruleset-query query magic --category world-building
```

**Success!** You now have a functioning RPG system with queryable rules.

---

## Writing Rules

### YAML Frontmatter Structure

Every rule file starts with YAML frontmatter:

```yaml
---
category: <category-path>       # Required: character-creation, world-building, or interactions
system: <system-id>              # Required: Your system ID
rules:                           # Required: Array of rules
  - id: <RULE-ID>                # Required: Unique ID (uppercase with dashes)
    version: <major.minor.patch> # Required: Semantic version
    changelog:                   # Required: At least one entry
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Description"
    tags: [tag1, tag2]           # Required: At least one tag
    visibility: public           # Required: public or gm-only

    # Optional fields
    title: "Optional Title"
    related: [OTHER-RULE-ID]
    conditions:
      - "character.level >= 5"
    formulas:
      formula_name: "1d6 + modifier"
    crossSystemRefs:
      - targetSystem: base-rpg
        targetRule: CORE-1.0
        refType: extends
---
```

### Markdown Content Structure

After frontmatter, write your rule content in Markdown:

```markdown
## Category Name

### [RULE-ID] Rule Title

Rule description in plain text.

**Formulas**:
- Attack: `1d20 + modifier`

**Conditions**:
- Character must be level 5+

**Example**: Concrete example of the rule in action

---

### [NEXT-RULE-ID] Next Rule

...
```

### Best Practices

1. **Use semantic versioning**: Major.minor.patch (1.0.0, 1.1.0, 2.0.0)
2. **Write clear changelog entries**: Explain why the change was made
3. **Tag consistently**: Use lowercase-with-dashes (combat, critical-hits, magic-system)
4. **Group related rules**: Put related rules in the same file
5. **Reference related rules**: Use the `related` field for cross-references
6. **Use visibility wisely**: Default to `public`, use `gm-only` for secrets
7. **Test formulas**: Validate formula syntax before committing

### Common Categories

```
character-creation/
  ├── attributes
  ├── combat
  ├── classes/
  │   ├── fighter
  │   ├── wizard
  │   └── rogue
  ├── ancestries
  └── equipment

world-building/
  ├── geography
  ├── magic-system
  ├── factions
  ├── history
  └── cosmology

interactions/
  ├── combat
  ├── social
  ├── exploration
  └── downtime
```

---

## Querying Rules

### Basic Queries

```bash
# Search all rules
rpg-ruleset-query query

# Keyword search
rpg-ruleset-query query combat critical

# Show full content
rpg-ruleset-query query combat --show-content
```

### Filtered Queries

```bash
# Filter by category
rpg-ruleset-query query --category character-creation

# Filter by subcategory
rpg-ruleset-query query --category character-creation/combat

# Filter by system
rpg-ruleset-query query --system tutorial-rpg

# Filter by tags
rpg-ruleset-query query --tag combat --tag critical-hits

# Combine filters
rpg-ruleset-query query magic \
  --category world-building \
  --system tutorial-rpg \
  --tag magic-system
```

### Advanced Queries

```bash
# GM-only rules
rpg-ruleset-query query secret --visibility gm-only

# Limit results
rpg-ruleset-query query combat --limit 5

# JSON output (for scripting)
rpg-ruleset-query query combat --format json | jq '.results[0].rule.id'

# Show formulas
rpg-ruleset-query query attack --show-formulas

# Show related rules
rpg-ruleset-query query COMBAT-1.0 --show-related
```

### Getting Rule Details

```bash
# Show full rule information
rpg-ruleset-query info COMBAT-1.0 --system tutorial-rpg

# Show changelog history
rpg-ruleset-query info COMBAT-1.0 --show-history

# JSON output
rpg-ruleset-query info COMBAT-1.0 --format json
```

---

## Validation Workflow

### Running Validation

```bash
# Validate a single file
rpg-ruleset-query validate rulesets/systems/my-rpg/character-creation/combat.md

# Validate with specific checklist
rpg-ruleset-query validate combat.md --checklist character-creation-validation

# Strict mode (warnings = errors)
rpg-ruleset-query validate combat.md --strict

# JSON output
rpg-ruleset-query validate combat.md --format json
```

### Understanding Validation Results

```
✓ [PASS] unique-rule-id
  All rule IDs are unique

✗ [ERROR] formula-syntax-valid
  Invalid dice notation: "2d" (missing die size)

⚠ [WARNING] tag-similarity
  Similar tags: "magic" and "magick" (90% similar)

⚪ [MANUAL] balance-review
  Rule requires manual review for balance
```

**Exit codes**:
- `0` = Validation passed (no errors)
- `1` = Validation failed (errors or warnings in strict mode)

### Pre-Commit Validation

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Validate changed rule files before commit

changed_files=$(git diff --cached --name-only --diff-filter=ACM | grep '\.md$')

for file in $changed_files; do
  if [[ $file == rulesets/systems/* ]]; then
    echo "Validating $file..."
    rpg-ruleset-query validate "$file" --strict
    if [ $? -ne 0 ]; then
      echo "Validation failed for $file"
      exit 1
    fi
  fi
done

echo "All validations passed!"
exit 0
```

---

## Multi-World Setup

### Creating a Base System

```bash
rpg-ruleset-query init base-fantasy-rpg \
  --name "Base Fantasy RPG" \
  --description "Core fantasy rules"
```

### Creating a Variant System

```bash
rpg-ruleset-query init dark-fantasy-variant \
  --name "Dark Fantasy Variant" \
  --base-system base-fantasy-rpg
```

This creates a variant with inheritance:

```yaml
# rulesets/systems/dark-fantasy-variant/system.yaml
systemId: dark-fantasy-variant
name: "Dark Fantasy Variant"
systemType: variant
baseSystem: base-fantasy-rpg
version:
  major: 1
  minor: 0
  patch: 0
```

### Extending Base Rules

In your variant, create `character-creation/combat.md`:

```markdown
---
category: character-creation/combat
system: dark-fantasy-variant
rules:
  - id: COMBAT-2.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T12:00:00Z
        changes: "Darker combat variant with wounds"
    tags: [combat, wounds, dark-theme]
    visibility: public
    crossSystemRefs:
      - targetSystem: base-fantasy-rpg
        targetRule: COMBAT-1.0
        refType: extends
    formulas:
      wound_threshold: "max_hp / 4"
---

## Wound System

### [COMBAT-2.0] Grievous Wounds

This rule extends [base-fantasy-rpg/COMBAT-1.0] with a wound system.

When you take damage exceeding your wound threshold (`max_hp / 4`):

**Effect**: Gain one level of exhaustion
```

### Querying Multi-World Systems

```bash
# Query both systems
rpg-ruleset-query query combat

# Query only base system
rpg-ruleset-query query combat --system base-fantasy-rpg

# Query only variant
rpg-ruleset-query query wounds --system dark-fantasy-variant
```

---

## Advanced Features

### Conditional Rules

Rules that apply only under certain conditions:

```markdown
---
rules:
  - id: STEALTH-1.0
    conditions:
      - "character.armor_type != 'heavy'"
      - "environment.lighting == 'dim' OR environment.lighting == 'darkness'"
    formulas:
      stealth_check: "1d20 + DEX_modifier + proficiency_bonus"
---

### [STEALTH-1.0] Stealth in Dim Light

**Conditions**:
- Not wearing heavy armor
- In dim light or darkness

**Check**: `1d20 + DEX_modifier + proficiency_bonus` vs passive Perception
```

### GM-Only Content

Rules visible only to Game Masters:

```markdown
---
rules:
  - id: SECRET-1.0
    visibility: gm-only
    tags: [plot, secrets, spoilers]
---

### [SECRET-1.0] The Ancient Prophecy

**GM ONLY**: This reveals the campaign's main plot twist.

The prophecy states that...
```

Query GM content:

```bash
rpg-ruleset-query query prophecy --visibility gm-only
```

### Exporting Content

```bash
# Export specific rules
rpg-ruleset-query export \
  --system my-rpg \
  --rules COMBAT-1.0 \
  --rules COMBAT-1.1 \
  combat-package.json

# Export entire category
rpg-ruleset-query export \
  --system my-rpg \
  --category character-creation/combat \
  --include-dependencies \
  combat-full.json
```

### Importing Content

```bash
# Dry run first
rpg-ruleset-query import \
  --system target-rpg \
  --dry-run \
  combat-package.json

# Import with conflict resolution
rpg-ruleset-query import \
  --system target-rpg \
  --resolve-conflicts rename \
  combat-package.json
```

---

## Tips & Tricks

### 1. Use Git for Version Control

```bash
cd rulesets/
git init
git add systems/
git commit -m "Initial ruleset commit"
```

### 2. Create Rule Templates

Save this as `templates/rule-template.md`:

```markdown
---
category: CHANGEME
system: CHANGEME
rules:
  - id: ID-1.0
    version: 1.0.0
    changelog:
      - version: 1.0.0
        date: 2025-11-16T10:00:00Z
        changes: "Initial version"
    tags: [CHANGEME]
    visibility: public
---

## Category Name

### [ID-1.0] Rule Title

Rule description here.
```

### 3. Use Consistent Naming

**Rule IDs**:
- Use category prefix: `COMBAT-`, `MAGIC-`, `STEALTH-`
- Use semantic versioning in ID if rules evolve: `COMBAT-1.0`, `COMBAT-2.0`

**Tags**:
- Use lowercase-with-dashes
- Be consistent: `magic-system` not `magic_system` or `MagicSystem`

**Categories**:
- Use lowercase-with-dashes
- Use hierarchical structure: `character-creation/combat/weapons`

### 4. Document Formulas

Always explain what formulas do:

```markdown
**Formula**: `1d20 + STR_modifier + proficiency_bonus`

Where:
- `1d20` = Attack die
- `STR_modifier` = Strength modifier (-5 to +10)
- `proficiency_bonus` = +2 to +6 based on level
```

---

## Next Steps

1. **Read the [Data Model](./data-model.md)** for complete entity definitions
2. **Review [CLI Interface](./contracts/cli-interface.md)** for all commands
3. **Check [Validation Checklists](./contracts/validation-checklists.md)** for quality guidelines
4. **Start building your system!**

---

## Troubleshooting

### Common Issues

**Q: Validation fails with "Rule ID not unique"**
A: Each rule ID must be unique within a system. Change the duplicate ID.

**Q: Query returns no results**
A: Check filters (category, system, tags). Try broadening the search.

**Q: Formula validation fails**
A: Ensure dice notation follows `XdY` format (e.g., `1d6`, `2d20`). Check for typos.

**Q: Related rule not found warning**
A: Referenced rule doesn't exist in the system. Either add the rule or remove the reference.

**Q: Cross-system reference fails**
A: Ensure the target system exists and the target rule ID is correct.

---

## Getting Help

- **Documentation**: Check `specs/001-dnd-ruleset-query/` for full docs
- **Examples**: See `rulesets/systems/tutorial-rpg/` for working examples
- **CLI Help**: `rpg-ruleset-query --help` or `rpg-ruleset-query COMMAND --help`

**Happy Rule Building!**
