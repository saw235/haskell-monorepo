# Data Model: Haskell Rich Terminal Library Enhancements

**Feature**: 003-haskell-rich-library
**Date**: 2025-11-15
**Purpose**: Define data structures and types for terminal detection and multi-line rendering features

## Overview

This document defines the data model for three new feature areas:
1. Terminal size detection
2. Unicode width calculation
3. Terminal capability detection
4. Multi-line cell rendering

All entities follow Haskell's pure functional design with clear separation between IO-based queries and pure rendering logic.

---

## 1. Terminal Size Detection

### Entity: TerminalSize

**Purpose**: Represents terminal dimensions (width and height in columns/rows)

**Structure**:
```haskell
data TerminalSize = TerminalSize
  { termWidth :: !Int   -- Number of columns (e.g., 80, 120, 200)
  , termHeight :: !Int  -- Number of rows (e.g., 24, 40, 60)
  } deriving (Show, Eq)
```

**Validation Rules**:
- Width and height MUST be positive integers (> 0)
- Typical width range: 40-500 columns
- Typical height range: 20-200 rows
- Invalid values should be rejected with Nothing/fallback

**State Transitions**: N/A (immutable value)

**Relationships**:
- Used by Table and Panel rendering to determine wrapping behavior
- Queried once at render time (cached in Console if performance-critical)

---

## 2. Unicode Width Calculation

### Entity: DisplayWidth

**Purpose**: Represents the visual display width of text in terminal columns (accounting for wide characters like CJK and emoji)

**Structure**:
```haskell
-- Not a separate type, just an Int, but conceptually:
-- type DisplayWidth = Int

-- Character widths:
-- 0 = Combining characters, control characters
-- 1 = Regular ASCII, most Latin characters
-- 2 = CJK characters, emoji, full-width characters
-- -1 = Unprintable (converted to 0 in safe calculation)
```

**Validation Rules**:
- Display width MUST be non-negative
- ANSI escape sequences do NOT contribute to width (must be stripped before calculation)
- Handle -1 return from wcwidth for unprintable characters
- Empty text has width 0

**Functions** (not entities, but key operations):
```haskell
displayWidth :: Text -> Int
-- Calculate total display width of text
-- Strips ANSI codes, sums character widths

safeDisplayWidth :: Text -> Int -> Int
-- Calculate width with fallback
-- Parameters: text, fallback_width
-- Returns: calculated width or fallback if invalid
```

**Relationships**:
- Used by Table column width calculation
- Used by Panel content alignment
- Used by Tree indentation

---

## 3. Terminal Capability Detection

### Entity: ColorCapability

**Purpose**: Represents the color rendering capabilities of the terminal

**Structure**:
```haskell
data ColorCapability
  = NoColor      -- Terminal doesn't support ANSI codes
  | Color16      -- Basic 16-color ANSI support
  | Color256     -- Extended 256-color support
  | TrueColor    -- RGB 24-bit color support
  deriving (Show, Eq, Ord, Enum)
```

**Validation Rules**:
- Capability levels are ordered: NoColor < Color16 < Color256 < TrueColor
- Detection is heuristic-based (may not be 100% accurate)
- When in doubt, assume lower capability for safety
- TERM=dumb always means NoColor

**State Transitions**:
```text
[Initial] → detectColorCapability → [Detected Capability]

Detection Logic:
  hSupportsANSIColor = True  → TrueColor (assume modern terminal)
  hSupportsANSI = True       → Color16 (basic ANSI only)
  Otherwise                  → NoColor (no support)
```

**Relationships**:
- Used by Style rendering to degrade colors appropriately
- Cached in Console handle to avoid repeated detection
- May be overridden by user preference (--no-color flag)

---

## 4. Multi-Line Cell Rendering

### Entity: MultiLineCell

**Purpose**: Represents a table cell that contains multiple lines of text

**Structure**:
```haskell
-- Extension to existing Cell type
data Cell = Cell
  { cellText :: Text      -- May contain newline characters (\n)
  , cellStyle :: Maybe Style
  } deriving (Show, Eq)

-- Internal helper for rendering
data CellLines = CellLines
  { cellLineCount :: !Int     -- Number of lines in this cell
  , cellMaxWidth :: !Int      -- Width of widest line (in columns)
  , cellSplitLines :: [Text]  -- Individual lines after splitting on \n
  } deriving (Show, Eq)
```

**Validation Rules**:
- Cell text MAY contain newline characters
- Maximum 50 lines per cell (configurable limit)
- Each line calculated independently for width
- Empty lines count toward line count
- Trailing newlines create empty lines

**Processing Logic**:
```text
Cell with "\n" → Split on newlines → Calculate width per line
                                  ↓
                  Find max width and line count
                                  ↓
              Determine row height (max of all cells in row)
                                  ↓
          Render each cell with vertical padding to match height
```

**Relationships**:
- Used by Table to expand row heights
- Requires DisplayWidth calculation per line
- Affects Table rendering performance (more lines = more work)

### Entity: TableRow (Extended)

**Purpose**: A row in a table, now supporting variable height

**Structure**:
```haskell
type Row = [Cell]  -- Existing definition

-- Internal rendering helper
data RenderedRow = RenderedRow
  { rowHeight :: !Int          -- Number of lines in tallest cell
  , rowCellLines :: [CellLines] -- Processed cells with split lines
  } deriving (Show, Eq)
```

**Validation Rules**:
- All cells in a row MUST render to same height (vertical padding)
- Row height = maximum of all cellLineCount values in row
- Empty cells still take up space (blank lines to match height)

---

## 5. Responsive Rendering

### Entity: RenderConstraints

**Purpose**: Constraints for rendering tables/panels within terminal bounds

**Structure**:
```haskell
data RenderConstraints = RenderConstraints
  { maxWidth :: !Int           -- Maximum width in columns
  , wrapMode :: WrapMode       -- How to handle overflow
  } deriving (Show, Eq)

data WrapMode
  = NoWrap                     -- Allow overflow (existing behavior)
  | Truncate                   -- Cut off at boundary with "..."
  | WrapContent                -- Wrap text within cells
  | ShrinkColumns              -- Reduce column widths proportionally
  deriving (Show, Eq, Enum)
```

**Validation Rules**:
- maxWidth MUST be positive
- maxWidth SHOULD be ≥ 40 (minimum usable terminal)
- Truncate mode adds ellipsis only if content actually truncated
- ShrinkColumns maintains minimum column width (e.g., 10 chars)

**State Transitions**: N/A (configuration value)

**Relationships**:
- Created from TerminalSize detection
- Passed to Table/Panel rendering functions
- Affects column width calculations

---

## 6. API Entities (Module Exports)

### Module: Rich.Terminal.Size

**Exports**:
```haskell
data TerminalSize = TerminalSize { termWidth :: Int, termHeight :: Int }

getTerminalSize :: IO (Maybe TerminalSize)
-- Query terminal size, returns Nothing if not a terminal

getTerminalWidth :: IO Int
-- Query width only, returns 80 if not a terminal (fallback)

getTerminalHeight :: IO Int
-- Query height only, returns 24 if not a terminal (fallback)
```

### Module: Rich.Terminal.Width

**Exports**:
```haskell
displayWidth :: Text -> Int
-- Calculate display width, handling wide characters

safeDisplayWidth :: Text -> Int
-- Like displayWidth but treats unprintable as width 0

calculateColumnWidth :: [Text] -> Int
-- Calculate max width for a column of text values
```

### Module: Rich.Terminal.Capability

**Exports**:
```haskell
data ColorCapability = NoColor | Color16 | Color256 | TrueColor

detectColorCapability :: IO ColorCapability
-- Detect terminal color support

supportsANSI :: IO Bool
-- Quick check for any ANSI support
```

### Module: Rich.Internal.MultiLine

**Exports** (internal only, not public API):
```haskell
splitCell :: Cell -> CellLines
-- Split multi-line cell into individual lines

calculateRowHeight :: Row -> Int
-- Determine height needed for a row

renderCellWithHeight :: Cell -> Int -> Align -> [Text]
-- Render cell to specific height with vertical padding
```

---

## 7. Backward Compatibility

**Existing API Compatibility**:
- All existing functions maintain current behavior
- New functionality is opt-in through new functions
- Table/Panel rendering without constraints works as before
- No breaking changes to existing types

**New vs Existing**:
```haskell
-- Existing (unchanged):
renderTable :: Table -> Text
renderPanel :: Panel -> Text

-- New (with responsive rendering):
renderTableWithWidth :: Table -> Int -> IO Text
renderPanelWithWidth :: Panel -> Int -> IO Text

-- New (fully automatic):
renderTableResponsive :: Table -> IO Text  -- Detects width automatically
renderPanelResponsive :: Panel -> IO Text
```

---

## 8. Constraints and Limits

| Entity | Constraint | Value | Rationale |
|--------|-----------|-------|-----------|
| TerminalSize | Min width | 40 columns | Minimum for usable table display |
| TerminalSize | Max width | 500 columns | Practical upper limit for terminals |
| TerminalSize | Min height | 20 rows | Standard minimum terminal height |
| MultiLineCell | Max lines | 50 lines | Prevent performance issues with huge cells |
| RenderConstraints | Min column width | 10 chars | Below this, content becomes unreadable |
| DisplayWidth | ANSI code width | 0 | Escape sequences don't count toward display |

---

## 9. Data Flow

### Terminal Size Detection Flow:
```text
User calls renderTableResponsive
    ↓
getTerminalSize :: IO (Maybe TerminalSize)
    ↓
Extract width (fallback to 80 if Nothing)
    ↓
Create RenderConstraints with maxWidth
    ↓
Pass to Table rendering with constraints
    ↓
Calculate column widths within bounds
    ↓
Render with wrapping/truncation if needed
```

### Multi-Line Cell Rendering Flow:
```text
Table with cells containing "\n"
    ↓
For each row: split all cells on newlines
    ↓
Calculate max line count in row (row height)
    ↓
For each cell: calculate display width per line
    ↓
For each cell: render all lines with padding to match row height
    ↓
Combine lines with proper borders/spacing
    ↓
Output multi-line row
```

### Color Degradation Flow:
```text
User renders styled text
    ↓
detectColorCapability :: IO ColorCapability
    ↓
Check capability level
    ↓
If TrueColor: Use RGB ANSI codes
If Color256:  Map RGB to nearest 256-color
If Color16:   Map RGB to nearest 16-color
If NoColor:   Strip all color codes
    ↓
Render with appropriate ANSI codes
```

---

## 10. Testing Strategy for Data Model

### Property-Based Tests (QuickCheck):
```haskell
prop "terminal size is positive" $ \size ->
  termWidth size > 0 && termHeight size > 0

prop "display width is non-negative" $ \text ->
  displayWidth text >= 0

prop "color capability is ordered" $ \cap1 cap2 ->
  cap1 <= cap2 ==> degradeColor cap2 cap1 (style)

prop "multi-line split preserves content" $ \cell ->
  T.concat (cellSplitLines (splitCell cell)) ==
    T.replace "\n" "" (cellText cell)
```

### Unit Tests:
```haskell
it "handles CJK characters correctly" $ do
  displayWidth "你好" `shouldBe` 4  -- 2 chars × 2 width

it "handles combining characters" $ do
  displayWidth "e\x0301" `shouldBe` 1  -- e + combining acute

it "splits multi-line cells correctly" $ do
  let cell = Cell "line1\nline2\nline3" Nothing
  cellLineCount (splitCell cell) `shouldBe` 3
```

---

## Summary

This data model provides:
- **Separation of concerns**: IO queries (terminal detection) vs pure rendering logic
- **Type safety**: Distinct types for different concepts (TerminalSize, ColorCapability)
- **Backward compatibility**: All new features are additive, no breaking changes
- **Testability**: Pure functions for most logic, mockable IO operations
- **Performance awareness**: Caching strategies, reasonable limits, efficient algorithms
