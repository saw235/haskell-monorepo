# Quick Start Guide: Rich Library Enhancements

**Feature**: 003-haskell-rich-library
**Date**: 2025-11-15
**Audience**: Developers using the Haskell Rich library

## Overview

This guide shows how to use the new terminal detection and responsive rendering features added to the Rich library. These enhancements include:

1. **Terminal Size Detection** - Automatically detect terminal dimensions
2. **Unicode Width Calculation** - Properly handle emoji, CJK characters, and wide characters
3. **Terminal Capability Detection** - Gracefully degrade colors for limited terminals
4. **Multi-Line Table Cells** - Tables that support cells with multiple lines
5. **Responsive Rendering** - Tables and panels that automatically fit terminal width

---

## Installation

### Add to BUILD.bazel

```haskell
haskell_binary(
    name = "my-app",
    srcs = ["Main.hs"],
    deps = [
        "//:base",
        "//:text",
        "//haskell/libs/rich",  # Rich library with new features
    ],
)
```

### Import Modules

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Rich  -- Core exports
import Rich.Terminal.Size
import Rich.Terminal.Width
import Rich.Terminal.Capability
```

---

## 1. Terminal Size Detection

### Basic Usage

```haskell
import Rich.Terminal.Size

main :: IO () = do
  maybeSize <- getTerminalSize
  case maybeSize of
    Just (TerminalSize width height) ->
      putStrLn $ "Terminal: " ++ show width ++ "x" ++ show height
    Nothing ->
      putStrLn "Not running in a terminal (output redirected?)"
```

**Output**:
```
Terminal: 120x40
```

### Quick Width Check

```haskell
main :: IO () = do
  width <- getTerminalWidth  -- Fallback to 80 if not a terminal
  putStrLn $ "Using width: " ++ show width
```

---

## 2. Responsive Table Rendering

### Automatic Width Detection

The simplest way to render tables that fit the terminal:

```haskell
import Rich

main :: IO () = do
  let table = simpleTable
        ["Name", "Email", "Department"]
        [ ["Alice Johnson", "alice@company.com", "Engineering"]
        , ["Bob Smith", "bob@company.com", "Marketing"]
        , ["Carol Davis", "carol@company.com", "Sales"]
        ]

  -- Automatically detects terminal width and adjusts
  rendered <- renderTableResponsive table
  putStrLn rendered
```

**Output** (on 80-column terminal):
```
┌───────────────┬─────────────────────┬─────────────┐
│ Name          │ Email               │ Department  │
├───────────────┼─────────────────────┼─────────────┤
│ Alice Johnson │ alice@company.com   │ Engineering │
│ Bob Smith     │ bob@company.com     │ Marketing   │
│ Carol Davis   │ carol@company.com   │ Sales       │
└───────────────┴─────────────────────┴─────────────┘
```

**Output** (on 40-column terminal - columns shrink):
```
┌──────┬───────────┬──────────┐
│ Name │ Email     │ Departm… │
├──────┼───────────┼──────────┤
│ Ali… │ alice@c…  │ Engine…  │
│ Bob… │ bob@com…  │ Market…  │
│ Car… │ carol@c…  │ Sales    │
└──────┴───────────┴──────────┘
```

### Manual Width Control

```haskell
main :: IO () = do
  let table = simpleTable ["Name", "Age"] [["Alice", "30"], ["Bob", "25"]]

  -- Render with specific width (e.g., for email or narrow display)
  putStrLn $ renderTableWithWidth table 50
```

---

## 3. Multi-Line Table Cells

### Tables with Wrapped Text

```haskell
import Rich
import Data.Text (Text)

main :: IO () = do
  let table = simpleTable
        ["Name", "Address", "Phone"]
        [ ["Alice", "123 Main Street\nApt 4B\nNew York, NY 10001", "555-1234"]
        , ["Bob", "456 Oak Avenue\nBoston, MA 02101", "555-5678"]
        ]

  putStrLn $ renderTable table
```

**Output**:
```
┌───────┬─────────────────────────┬──────────┐
│ Name  │ Address                 │ Phone    │
├───────┼─────────────────────────┼──────────┤
│ Alice │ 123 Main Street         │ 555-1234 │
│       │ Apt 4B                  │          │
│       │ New York, NY 10001      │          │
├───────┼─────────────────────────┼──────────┤
│ Bob   │ 456 Oak Avenue          │ 555-5678 │
│       │ Boston, MA 02101        │          │
└───────┴─────────────────────────┴──────────┘
```

**Key Points**:
- Each `\n` creates a new line within the cell
- Row height automatically expands to tallest cell
- Other cells in the row are vertically padded
- Content remains aligned

---

## 4. Unicode Width Handling

### Correct CJK and Emoji Display

```haskell
import Rich.Terminal.Width

main :: IO () = do
  let texts = ["Hello", "你好", "😀👍", "Café"]

  putStrLn "Text Widths:"
  mapM_ (\t -> do
    let w = displayWidth t
    putStrLn $ t ++ " -> " ++ show w ++ " columns"
    ) texts
```

**Output**:
```
Text Widths:
Hello -> 5 columns
你好 -> 4 columns      (2 chars × 2 width each)
😀👍 -> 4 columns      (2 emoji × 2 width each)
Café -> 4 columns      (4 characters)
```

### Table with CJK Characters

```haskell
main :: IO () = do
  let table = simpleTable
        ["Name", "City"]
        [ ["Alice", "New York"]
        , ["田中", "東京"]      -- Japanese name and city
        , ["李明", "北京"]      -- Chinese name and city
        ]

  putStrLn $ renderTable table
```

**Output** (correctly aligned):
```
┌───────┬──────────┐
│ Name  │ City     │
├───────┼──────────┤
│ Alice │ New York │
│ 田中  │ 東京     │
│ 李明  │ 北京     │
└───────┴──────────┘
```

---

## 5. Color Capability Detection

### Adaptive Color Rendering

```haskell
import Rich
import Rich.Terminal.Capability

main :: IO () = do
  capability <- detectColorCapability

  console <- newConsole

  case capability of
    TrueColor -> do
      printStyledLn console (rgb 100 150 200 $ style) "RGB colors supported!"

    Color16 -> do
      printStyledLn console (blue $ style) "16-color mode (basic ANSI)"

    NoColor -> do
      printLn console "No color support (plain text)"
```

### Graceful Degradation

```haskell
renderStatus :: Text -> IO ()
renderStatus status = do
  cap <- detectColorCapability
  console <- newConsole

  let statusStyle = case status of
        "success" -> case cap of
          TrueColor -> rgb 0 200 0 $ bold style    -- Bright green RGB
          Color16 -> green $ bold style             -- Standard green
          NoColor -> bold style                     -- Just bold
        "error" -> case cap of
          TrueColor -> rgb 200 0 0 $ bold style    -- Bright red RGB
          Color16 -> red $ bold style              -- Standard red
          NoColor -> bold style                    -- Just bold
        _ -> style

  printStyledLn console statusStyle status
```

---

## 6. Responsive Panels

### Panels that Fit Terminal

```haskell
import Rich

main :: IO () = do
  let longMessage = "This is a very long message that might need to wrap to " <>
                    "fit within the terminal width. The panel will automatically " <>
                    "adjust its width based on terminal size."

  let panel = simplePanel "Important Notice" longMessage

  -- Automatically fits terminal width
  rendered <- renderPanelResponsive panel
  putStrLn rendered
```

**Output** (on narrow terminal):
```
╭─ Important Notice ─────────────╮
│                                │
│  This is a very long message   │
│  that might need to wrap to    │
│  fit within the terminal        │
│  width. The panel will auto…    │
│                                │
╰────────────────────────────────╯
```

---

## 7. Combined Example: Complete CLI App

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Rich
import Rich.Terminal.Size
import Rich.Terminal.Capability
import Data.Text (Text)
import qualified Data.Text as T

data User = User
  { userName :: Text
  , userEmail :: Text
  , userStatus :: Text
  }

main :: IO () = do
  console <- newConsole

  -- Print header with capability-aware styling
  capability <- detectColorCapability
  let headerStyle = case capability of
        TrueColor -> rgb 70 130 180 $ bold style
        Color16 -> cyan $ bold style
        NoColor -> bold style

  printStyledLn console headerStyle "=== User Management System ==="
  emptyLine console

  -- Display system info
  width <- getTerminalWidth
  printInfo console $ "Terminal width: " <> T.pack (show width) <> " columns"

  -- Create user table with multi-line addresses
  let users =
        [ User "Alice Johnson" "alice@company.com\nalice.j@personal.com" "Active"
        , User "Bob Smith" "bob@company.com" "Pending"
        , User "Carol Davis" "carol@company.com\ncarol.davis@backup.com" "Active"
        ]

  let table = simpleTable
        ["Name", "Email(s)", "Status"]
        [ [userName u, userEmail u, userStatus u] | u <- users ]

  -- Render responsively
  renderedTable <- renderTableResponsive table
  putStrLn renderedTable

  emptyLine console

  -- Show warning in panel
  let warning = "Some users have multiple email addresses.\nPlease verify contact information."
  let warningPanel = setBorderStyle DoubleBorder $ simplePanel "⚠ Notice" warning

  renderedPanel <- renderPanelResponsive warningPanel
  putStrLn renderedPanel

  -- Success message
  printSuccess console "User data loaded successfully"
```

**Output**:
```
=== User Management System ===

ℹ Terminal width: 120 columns

┌────────────────┬──────────────────────────────┬─────────┐
│ Name           │ Email(s)                     │ Status  │
├────────────────┼──────────────────────────────┼─────────┤
│ Alice Johnson  │ alice@company.com            │ Active  │
│                │ alice.j@personal.com         │         │
├────────────────┼──────────────────────────────┼─────────┤
│ Bob Smith      │ bob@company.com              │ Pending │
├────────────────┼──────────────────────────────┼─────────┤
│ Carol Davis    │ carol@company.com            │ Active  │
│                │ carol.davis@backup.com       │         │
└────────────────┴──────────────────────────────┴─────────┘

╔═ ⚠ Notice ════════════════════════════════════════════╗
║                                                       ║
║  Some users have multiple email addresses.           ║
║  Please verify contact information.                  ║
║                                                       ║
╚═══════════════════════════════════════════════════════╝

✓ User data loaded successfully
```

---

## 8. Migration from Existing Code

### Before (without responsive features):

```haskell
main :: IO () = do
  let table = simpleTable headers rows
  putStrLn $ renderTable table  -- Might overflow on narrow terminals
```

### After (with responsive features):

**Option 1: Fully automatic (recommended)**
```haskell
main :: IO () = do
  let table = simpleTable headers rows
  rendered <- renderTableResponsive table  -- Detects width automatically
  putStrLn rendered
```

**Option 2: Manual control**
```haskell
main :: IO () = do
  let table = simpleTable headers rows
  width <- getTerminalWidth
  putStrLn $ renderTableWithWidth table (width - 4)  -- Leave margin
```

**Option 3: Fixed width (e.g., for email)**
```haskell
main :: IO () = do
  let table = simpleTable headers rows
  putStrLn $ renderTableWithWidth table 72  -- Email-friendly width
```

---

## 9. Best Practices

### 1. Cache Terminal Size

```haskell
-- ✅ Good: Query once, use many times
main :: IO () = do
  termWidth <- getTerminalWidth

  putStrLn $ renderTableWithWidth table1 termWidth
  putStrLn $ renderTableWithWidth table2 termWidth
  putStrLn $ renderPanelWithWidth panel termWidth

-- ❌ Avoid: Repeated queries
main :: IO () = do
  putStrLn =<< renderTableResponsive table1  -- Query 1
  putStrLn =<< renderTableResponsive table2  -- Query 2
  putStrLn =<< renderPanelResponsive panel   -- Query 3
```

### 2. Handle Non-Terminal Output

```haskell
main :: IO () = do
  maybeSize <- getTerminalSize
  let width = case maybeSize of
        Just size -> termWidth size
        Nothing -> 120  -- Wider default for piped output

  putStrLn $ renderTableWithWidth table width
```

### 3. Test with Different Terminal Sizes

```bash
# Test narrow terminal (80 columns)
tput cols 80 && ./my-app

# Test wide terminal (200 columns)
tput cols 200 && ./my-app

# Test redirected output (not a terminal)
./my-app > output.txt && cat output.txt
```

### 4. Use Unicode Width for Custom Alignment

```haskell
import Rich.Terminal.Width

centerText :: Int -> Text -> Text
centerText targetWidth text =
  let actualWidth = displayWidth text
      padding = max 0 (targetWidth - actualWidth)
      leftPad = padding `div` 2
      rightPad = padding - leftPad
  in T.replicate leftPad " " <> text <> T.replicate rightPad " "

-- Works correctly with emoji and CJK:
centerText 20 "Hello"     -- "       Hello        "
centerText 20 "你好世界"  -- "      你好世界      "
centerText 20 "🎉 Party"  -- "     🎉 Party      "
```

---

## 10. Troubleshooting

### Problem: Table still overflows terminal

**Solution**: Check that you're using responsive functions
```haskell
-- ✅ Correct
rendered <- renderTableResponsive table

-- ❌ Wrong (doesn't adapt)
putStrLn $ renderTable table
```

### Problem: CJK characters misaligned

**Solution**: Library handles this automatically. If you see issues, ensure:
- Terminal supports UTF-8 (`echo $LANG` should show UTF-8)
- Font has CJK glyphs
- Using `displayWidth` for custom width calculations

### Problem: Colors don't show

**Solution**: Check terminal capability
```haskell
cap <- detectColorCapability
print cap  -- Should show Color16 or better
```

If shows `NoColor`, check:
- `echo $TERM` (should not be "dumb")
- stdout is a terminal (not piped)
- Terminal emulator supports ANSI

### Problem: Multi-line cells don't work

**Solution**: Ensure you're using `\n` for newlines
```haskell
-- ✅ Correct
Cell "Line 1\nLine 2" Nothing

-- ❌ Wrong (literal backslash-n)
Cell "Line 1\\nLine 2" Nothing
```

---

## Summary

The Rich library enhancements provide:
- ✅ Automatic terminal size detection
- ✅ Responsive table and panel rendering
- ✅ Correct Unicode width handling (CJK, emoji)
- ✅ Graceful color degradation
- ✅ Multi-line table cells
- ✅ Backward-compatible API

**Next Steps**:
- Read [api-contracts.md](contracts/api-contracts.md) for detailed API documentation
- See [data-model.md](data-model.md) for internal data structures
- Check [research.md](research.md) for dependency information

**Feedback**: Report issues or suggest improvements at the project repository.
