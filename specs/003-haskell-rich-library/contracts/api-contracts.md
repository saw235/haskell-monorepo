# API Contracts: Haskell Rich Terminal Library

**Feature**: 003-haskell-rich-library
**Date**: 2025-11-15
**Type**: Haskell Library API (not REST/GraphQL)

## Overview

This document defines the public API contracts for the Rich library enhancements. Since this is a Haskell library, contracts are defined as module exports with type signatures, behavioral guarantees, and usage examples.

---

## Module: Rich.Terminal.Size

**Purpose**: Query terminal dimensions for responsive rendering

### Public API

```haskell
module Rich.Terminal.Size
  ( -- * Types
    TerminalSize(..)
  , -- * Queries
    getTerminalSize
  , getTerminalWidth
  , getTerminalHeight
  ) where
```

### Type Contracts

#### TerminalSize

```haskell
data TerminalSize = TerminalSize
  { termWidth :: Int   -- ^ Terminal width in columns (positive integer)
  , termHeight :: Int  -- ^ Terminal height in rows (positive integer)
  } deriving (Show, Eq)
```

**Invariants**:
- `termWidth > 0`
- `termHeight > 0`
- Typical ranges: width 40-500, height 20-200

### Function Contracts

#### getTerminalSize

```haskell
getTerminalSize :: IO (Maybe TerminalSize)
```

**Contract**:
- **Preconditions**: None
- **Postconditions**:
  - Returns `Just size` if stdout is a terminal
  - Returns `Nothing` if stdout is redirected or not a terminal
  - If `Just size` returned, both width and height are positive
- **Side Effects**: Performs ioctl syscall (Unix) or Console API call (Windows)
- **Thread Safety**: Safe (read-only system call)
- **Performance**: O(1), ~0.01ms

**Example**:
```haskell
main :: IO () = do
  maybeSize <- getTerminalSize
  case maybeSize of
    Just (TerminalSize w h) -> putStrLn $ "Terminal: " ++ show w ++ "x" ++ show h
    Nothing -> putStrLn "Not a terminal"
```

#### getTerminalWidth

```haskell
getTerminalWidth :: IO Int
```

**Contract**:
- **Preconditions**: None
- **Postconditions**:
  - Returns actual terminal width if available
  - Returns 80 (standard default) if not a terminal
  - Result is always positive
- **Side Effects**: Same as `getTerminalSize`
- **Fallback**: 80 columns (standard terminal width)

**Example**:
```haskell
renderTable :: Table -> IO Text
renderTable table = do
  width <- getTerminalWidth
  return $ renderTableWithWidth table width
```

#### getTerminalHeight

```haskell
getTerminalHeight :: IO Int
```

**Contract**:
- **Preconditions**: None
- **Postconditions**:
  - Returns actual terminal height if available
  - Returns 24 (standard default) if not a terminal
  - Result is always positive
- **Side Effects**: Same as `getTerminalSize`
- **Fallback**: 24 rows (standard terminal height)

---

## Module: Rich.Terminal.Width

**Purpose**: Calculate display width of Unicode text

### Public API

```haskell
module Rich.Terminal.Width
  ( displayWidth
  , safeDisplayWidth
  , displayWidthIgnoringANSI
  ) where
```

### Function Contracts

#### displayWidth

```haskell
displayWidth :: Text -> Int
```

**Contract**:
- **Preconditions**: None (handles all Text input)
- **Postconditions**:
  - Returns non-negative integer
  - ASCII characters count as 1
  - CJK/emoji count as 2
  - Combining characters count as 0
  - Unprintable characters count as 0 (not -1)
- **Side Effects**: None (pure function)
- **Performance**: O(n) where n = text length, ~100ns per character

**Width Rules**:
```haskell
displayWidth "hello"    == 5   -- Regular ASCII
displayWidth "你好"      == 4   -- 2 CJK chars × 2
displayWidth "😀"        == 2   -- Emoji (wide character)
displayWidth "e\x0301"  == 1   -- e + combining acute accent
displayWidth "\t"       == 0   -- Tab (control character, unprintable)
```

**Example**:
```haskell
alignCenter :: Int -> Text -> Text
alignCenter targetWidth text =
  let actualWidth = displayWidth text
      padding = max 0 (targetWidth - actualWidth)
      leftPad = padding `div` 2
      rightPad = padding - leftPad
  in T.replicate leftPad " " <> text <> T.replicate rightPad " "
```

#### safeDisplayWidth

```haskell
safeDisplayWidth :: Text -> Int
```

**Contract**:
- **Preconditions**: None
- **Postconditions**: Same as `displayWidth`
- **Difference**: Uses safe variant of wcwidth that treats unknowns as width 0
- **Use When**: Processing untrusted input that may contain unusual Unicode

#### displayWidthIgnoringANSI

```haskell
displayWidthIgnoringANSI :: Text -> Int
```

**Contract**:
- **Preconditions**: Text may contain ANSI escape sequences
- **Postconditions**:
  - Strips all ANSI escape codes before calculation
  - ANSI codes contribute 0 to width
  - Returns width of visible text only
- **Side Effects**: None (pure function)
- **Performance**: O(n), with small overhead for ANSI stripping

**Example**:
```haskell
displayWidthIgnoringANSI "\ESC[31mRed Text\ESC[0m" == 8  -- Only "Red Text" counts
```

---

## Module: Rich.Terminal.Capability

**Purpose**: Detect terminal color support

### Public API

```haskell
module Rich.Terminal.Capability
  ( -- * Types
    ColorCapability(..)
  , -- * Detection
    detectColorCapability
  , detectColorCapabilityFor
  , supportsANSI
  ) where
```

### Type Contracts

#### ColorCapability

```haskell
data ColorCapability
  = NoColor      -- ^ No ANSI support (TERM=dumb or not a terminal)
  | Color16      -- ^ Basic 16-color ANSI
  | Color256     -- ^ Extended 256-color palette
  | TrueColor    -- ^ Full RGB 24-bit color
  deriving (Show, Eq, Ord, Enum, Bounded)
```

**Invariants**:
- Ordered from least to most capable: `NoColor < Color16 < Color256 < TrueColor`
- Can use `Ord` to check capability: `cap >= Color16` means color supported

### Function Contracts

#### detectColorCapability

```haskell
detectColorCapability :: IO ColorCapability
```

**Contract**:
- **Preconditions**: None
- **Postconditions**:
  - Returns `NoColor` if stdout not a terminal or `TERM=dumb`
  - Returns `Color16` if basic ANSI supported but not full color
  - Returns `TrueColor` if modern terminal (heuristic: assumes RGB support)
  - Result is deterministic for same environment
- **Side Effects**: Reads `TERM` environment variable, queries stdout handle
- **Thread Safety**: Safe (read-only operations)
- **Detection Method**: Heuristic-based (see data-model.md for logic)

**Detection Heuristics**:
```text
hSupportsANSIColor(stdout) == True  → TrueColor (assume modern terminal)
hSupportsANSI(stdout) == True       → Color16 (basic support only)
Otherwise                           → NoColor
```

**Example**:
```haskell
printWithColor :: Text -> IO ()
printWithColor text = do
  capability <- detectColorCapability
  let styled = case capability of
        TrueColor -> rgb 100 150 200 style
        Color16 -> blue style
        NoColor -> style
  putStrLn $ renderText styled text
```

#### detectColorCapabilityFor

```haskell
detectColorCapabilityFor :: Handle -> IO ColorCapability
```

**Contract**:
- **Preconditions**: Handle must be valid (not closed)
- **Postconditions**: Same as `detectColorCapability` but for specific handle
- **Use Case**: Detect capability for stderr separately from stdout

**Example**:
```haskell
printError :: Text -> IO ()
printError msg = do
  stderrCap <- detectColorCapabilityFor stderr
  let color = if stderrCap >= Color16 then red style else style
  hPutStrLn stderr $ renderText color msg
```

#### supportsANSI

```haskell
supportsANSI :: IO Bool
```

**Contract**:
- **Preconditions**: None
- **Postconditions**:
  - Returns `True` if any ANSI support (Color16 or better)
  - Returns `False` if NoColor
  - Equivalent to: `detectColorCapability >>= \cap -> return (cap /= NoColor)`
- **Side Effects**: Same as `detectColorCapability`

---

## Module: Rich.Table (Extended)

**Purpose**: Extended table rendering with multi-line cells and responsive layout

### Extended API

```haskell
-- Existing exports (unchanged):
renderTable :: Table -> Text

-- New exports:
renderTableWithWidth :: Table -> Int -> Text
renderTableResponsive :: Table -> IO Text
setMaxWidth :: Int -> Table -> Table
```

### Function Contracts

#### renderTableWithWidth

```haskell
renderTableWithWidth :: Table -> Int -> Text
```

**Contract**:
- **Preconditions**:
  - `maxWidth >= 40` (minimum usable width)
  - Table must be valid (matching column/row counts)
- **Postconditions**:
  - All rendered lines have length ≤ maxWidth
  - Columns shrink proportionally if needed
  - Multi-line cells rendered with proper height alignment
  - No line exceeds specified width
- **Side Effects**: None (pure function)
- **Performance**: O(rows × cols × max_cell_content)

**Behavior**:
```haskell
-- If table naturally fits:
renderTableWithWidth bigTable 200 == renderTable bigTable

-- If table is too wide:
-- - Columns shrink proportionally
-- - Minimum column width maintained (10 chars)
-- - Ellipsis added to truncated content
```

**Example**:
```haskell
displayTable :: Table -> IO ()
displayTable table = do
  width <- getTerminalWidth
  putStrLn $ renderTableWithWidth table (width - 2)  -- Leave 2-char margin
```

#### renderTableResponsive

```haskell
renderTableResponsive :: Table -> IO Text
```

**Contract**:
- **Preconditions**: Table must be valid
- **Postconditions**:
  - Automatically detects terminal width
  - Falls back to width 80 if not a terminal
  - Equivalent to: `getTerminalWidth >>= renderTableWithWidth table`
- **Side Effects**: Queries terminal size (IO)
- **Use Case**: Simplest API for automatic responsive rendering

**Example**:
```haskell
main :: IO () = do
  let table = simpleTable ["Name", "Email", "Status"]
                          [["Alice", "alice@example.com", "Active"]]
  rendered <- renderTableResponsive table
  putStrLn rendered
```

#### setMaxWidth

```haskell
setMaxWidth :: Int -> Table -> Table
```

**Contract**:
- **Preconditions**: `width >= 40`
- **Postconditions**: Returns table configured with max width constraint
- **Side Effects**: None (pure function)
- **Use Case**: Set width constraint before rendering with `renderTable`

---

## Module: Rich.Panel (Extended)

**Purpose**: Extended panel rendering with responsive layout

### Extended API

```haskell
-- Existing exports (unchanged):
renderPanel :: Panel -> Text

-- New exports:
renderPanelWithWidth :: Panel -> Int -> Text
renderPanelResponsive :: Panel -> IO Text
```

### Function Contracts

#### renderPanelWithWidth

```haskell
renderPanelWithWidth :: Panel -> Int -> Text
```

**Contract**:
- **Preconditions**:
  - `maxWidth >= 20` (minimum for panel + borders)
  - Panel must be valid
- **Postconditions**:
  - Panel width ≤ maxWidth
  - Content wraps within available space
  - Border characters properly aligned
  - Multi-line content handled correctly
- **Side Effects**: None (pure function)

**Example**:
```haskell
showMessage :: Text -> IO ()
showMessage msg = do
  width <- getTerminalWidth
  let panel = simplePanel "Notice" msg
  putStrLn $ renderPanelWithWidth panel width
```

#### renderPanelResponsive

```haskell
renderPanelResponsive :: Panel -> IO Text
```

**Contract**:
- **Preconditions**: Panel must be valid
- **Postconditions**:
  - Auto-detects terminal width
  - Falls back to 80 if not a terminal
  - Equivalent to: `getTerminalWidth >>= renderPanelWithWidth panel`
- **Side Effects**: Queries terminal size (IO)

---

## Multi-Line Cell Support

### Behavior Contract

**Input**: Cell with newline characters
```haskell
Cell "Line 1\nLine 2\nLine 3" Nothing
```

**Output**: Row with height = 3
```text
┌─────────┐
│ Line 1  │
│ Line 2  │
│ Line 3  │
└─────────┘
```

**Alignment Rules**:
- Multi-line cells vertically align: top-aligned by default
- All cells in row rendered to same height (tallest cell determines height)
- Empty lines preserved (e.g., "A\n\nB" has 3 lines)
- Trailing newlines create empty lines

**Example**:
```haskell
let table = simpleTable
      ["Name", "Address"]
      [ [Cell "Alice" Nothing, Cell "123 Main St\nApt 4B\nNew York, NY" Nothing]
      , [Cell "Bob" Nothing, Cell "456 Oak Ave\nBoston, MA" Nothing]
      ]

-- First row has height 3 (Address has 3 lines)
-- Second row has height 2 (Address has 2 lines)
```

---

## Backward Compatibility Guarantees

### Existing Functions (Unchanged Behavior)

All existing functions maintain their current contracts:
- `renderTable :: Table -> Text` - No change
- `renderPanel :: Panel -> Text` - No change
- `renderTree :: Tree -> Text` - No change
- `renderProgress :: ProgressBar -> Text` - No change

**If cell text contains `\n` in existing code**:
- Previous behavior: Undefined (likely rendered as-is or broke layout)
- New behavior: Properly handled as multi-line cell
- **Impact**: Bug fix, not breaking change

### New Functions (Opt-In)

- `renderTableResponsive` - New function, opt-in
- `renderTableWithWidth` - New function, opt-in
- Terminal detection modules - New modules, no impact on existing code

**Migration Path**:
```haskell
-- Old code (still works):
putStrLn $ renderTable myTable

-- New code (responsive):
rendered <- renderTableResponsive myTable
putStrLn rendered

-- Manual width control:
putStrLn $ renderTableWithWidth myTable 120
```

---

## Error Handling Contracts

### Invalid Input Handling

| Function | Invalid Input | Behavior |
|----------|--------------|----------|
| `getTerminalSize` | Not a terminal | Returns `Nothing` |
| `getTerminalWidth` | Not a terminal | Returns 80 (fallback) |
| `displayWidth` | Invalid Unicode | Treats as width 0 |
| `renderTableWithWidth` | width < 40 | Uses minimum width 40 |
| `renderTableWithWidth` | Empty table | Returns empty string (no crash) |
| `detectColorCapability` | No TERM variable | Returns NoColor |

### Exception Safety

**No exceptions thrown by library** (except for system-level IO errors like SIGPIPE):
- All functions handle edge cases gracefully
- Invalid input returns sensible defaults
- No partial results or corrupt output

---

## Performance Contracts

| Operation | Time Complexity | Expected Duration |
|-----------|----------------|-------------------|
| `getTerminalSize` | O(1) | ~0.01ms |
| `displayWidth` | O(n) chars | ~100ns/char |
| `detectColorCapability` | O(1) | ~0.1ms |
| `renderTableWithWidth` | O(rows × cols × content) | <50ms for 100 rows |
| `renderPanelWithWidth` | O(lines × content) | <10ms for typical panels |

**Caching Recommendations**:
- Cache `TerminalSize` in application (doesn't change during execution)
- Cache `ColorCapability` in Console handle (one detection per session)
- Don't cache rendered output (content may change)

---

## Thread Safety

**All query functions are thread-safe**:
- `getTerminalSize` - Safe (read-only syscall)
- `detectColorCapability` - Safe (read-only env/handle queries)

**Pure rendering functions are thread-safe**:
- `renderTable`, `renderPanel`, etc. - Pure functions, no shared state

**Output functions require external synchronization**:
- Concurrent `putStrLn` calls may interleave
- Use `MVar` or `Lock` if multiple threads print simultaneously

---

## Testing Contracts

### Testability Guarantees

**All pure functions are testable without IO**:
```haskell
-- No IO needed:
displayWidth "test" `shouldBe` 4
renderTableWithWidth table 80 `shouldContain` "Alice"
```

**IO functions are mockable**:
```haskell
-- Can mock terminal size:
withMockedTerminal (TerminalSize 120 40) $ do
  width <- getTerminalWidth
  width `shouldBe` 120
```

### QuickCheck Properties

Library guarantees these properties hold for all inputs:
```haskell
prop "display width is non-negative" $ \text ->
  displayWidth text >= 0

prop "terminal width is positive" $ \size ->
  termWidth size > 0

prop "color capability is ordered" $ \cap1 cap2 ->
  cap1 <= cap2 || cap2 <= cap1  -- Total order
```

---

## Summary

This API contract document provides:
- **Type signatures** with precise contracts
- **Behavioral guarantees** (preconditions, postconditions, invariants)
- **Performance expectations** (time complexity, typical durations)
- **Backward compatibility** (existing code unaffected)
- **Error handling** (no exceptions, graceful degradation)
- **Testing support** (pure functions, mockable IO)
