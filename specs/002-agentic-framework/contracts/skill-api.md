# Skill API Contract

**Module**: `AgenticFramework.Skill`
**Purpose**: Skill loading, discovery, and template generation
**Version**: 1.0.0

---

## Core Functions

### loadSkillsFromDirectory

**Signature**:
```haskell
loadSkillsFromDirectory :: FilePath -> IO [Skill]
```

**Description**: Loads all skill files from directory (labels and descriptions only, lazy content loading).

**Behavior** (FR-012b):
1. Scans directory for `.md` files
2. Parses YAML front-matter (label, description, category)
3. Returns `Skill` instances with `NotLoaded` content
4. Full content loaded on-demand via `loadSkillContent`

**Example**:
```haskell
skills <- loadSkillsFromDirectory "./skills"
putStrLn $ "Loaded " <> show (length skills) <> " skills"
```

**Fulfills**: FR-012, FR-012a, FR-012b

---

### loadSkillContent

**Signature**:
```haskell
loadSkillContent :: Skill -> IO Skill
```

**Description**: Loads full markdown content for a skill (if not already loaded).

**Behavior**:
- If `skillContent == NotLoaded`, reads file and parses markdown
- If already `Loaded`, returns skill unchanged (idempotent)
- Parses sections: Purpose, Methodology, Examples

**Example**:
```haskell
-- Lazy loading pattern
let skill = findSkillByLabel skills "debugging-checklist"
fullSkill <- loadSkillContent skill
case skillContent fullSkill of
  Loaded markdown -> putStrLn $ purpose markdown
  _ -> error "Content should be loaded"
```

---

### searchSkills

**Signature**:
```haskell
searchSkills :: [Skill] -> SkillQuery -> [Skill]
```

**Description**: Searches skills by label, description, or category.

**Parameters**:
```haskell
data SkillQuery
  = ByLabel Text
  | ByCategory SkillCategory
  | ByKeyword Text              -- Searches description
  | Combined [SkillQuery]       -- AND logic
```

**Example**:
```haskell
-- Find debugging skills
let debugSkills = searchSkills skills (ByCategory "debugging")

-- Find specific skill
let checklistSkill = searchSkills skills (ByLabel "debugging-checklist")
```

**Fulfills**: FR-015

---

### generateSkillTemplate

**Signature**:
```haskell
generateSkillTemplate :: SkillLabel -> SkillCategory -> FilePath -> IO ()
```

**Description**: Generates properly structured skill file scaffold.

**Output Format**:
```markdown
---
label: my-skill
description: Brief description here
category: testing
---

## Purpose
What problem does this skill address?

## Methodology
1. Step one
2. Step two
...

## Examples
### Example 1: Scenario Name
...
```

**Example**:
```haskell
generateSkillTemplate
  (SkillLabel "code-review-checklist")
  (SkillCategory "code-review")
  "./skills/code-review-checklist.md"
```

**Fulfills**: FR-012c

---

### watchSkillsDirectory

**Signature**:
```haskell
watchSkillsDirectory :: FilePath -> ([Skill] -> IO ()) -> IO ()
```

**Description**: Watches skills directory for changes, reloads on file modification.

**Behavior** (FR-016):
- Uses file system watcher (inotify on Linux, FSEvents on macOS)
- On file change: Reloads affected skill, calls callback with updated skill list
- Non-blocking: Runs watcher in background thread

**Example**:
```haskell
watchSkillsDirectory "./skills" $ \updatedSkills -> do
  putStrLn $ "Skills reloaded: " <> show (length updatedSkills)
  -- Update agent's available skills
```

---

## Skill File Format

### Structure (FR-012a)

Required front-matter:
```yaml
---
label: unique-identifier          # REQUIRED
description: Brief summary        # REQUIRED
category: skill-category          # OPTIONAL
---
```

Recommended content sections:
- `## Purpose` - What the skill addresses
- `## Methodology` - Step-by-step approach
- `## Examples` - Concrete demonstrations

**Validation**:
- Label must be unique within directory
- Description cannot be empty
- Category is optional but recommended
- Content structure flexible (only headers recommended, not enforced)

---

## Making Skills Available to Agents

### addSkillToAgent

**Signature**:
```haskell
addSkillToAgent :: Skill -> Agent -> Agent
```

**Description**: Adds skill to agent's available skills (updates AgentContext on execution).

**Example**:
```haskell
skills <- loadSkillsFromDirectory "./skills"
let debugSkill = head $ searchSkills skills (ByLabel "debugging-checklist")
let agentWithSkill = addSkillToAgent debugSkill agent
```

---

### skillAsTool

**Signature**:
```haskell
skillAsTool :: Skill -> Tool
```

**Description**: Converts skill to a tool that agents can explicitly invoke.

**Behavior**:
- Tool name: "use_skill_{label}"
- Tool description: Skill's description
- Tool execution: Loads full content and injects into agent context

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "query": { "type": "string", "description": "Optional query to focus skill application" }
  }
}
```

**Output Schema**:
```json
{
  "type": "object",
  "properties": {
    "skill_content": { "type": "string" },
    "skill_label": { "type": "string" }
  }
}
```

**Example**:
```haskell
let debugTool = skillAsTool debugSkill
let agent = createAgent config { configTools = [debugTool, ...] }
```

**Fulfills**: FR-013

---

## Performance Characteristics

- **Initial Load** (labels only): O(n) where n = number of files, <100ms for 100 skills
- **Content Loading**: O(1) per skill, 1-10ms per file
- **Search**: O(n) linear scan, <1ms for 100 skills
- **File Watch**: Negligible overhead, event-driven

**Fulfills**: SC-007 (<500ms overhead)

---

## Thread Safety

- `Skill` instances are immutable and thread-safe
- `loadSkillContent` can be called concurrently (file I/O is thread-safe)
- `watchSkillsDirectory` uses separate thread, callback must be thread-safe

---

**See Also**:
- `agent-api.md` - Adding skills to agents
- `data-model.md` - Skill entity details
