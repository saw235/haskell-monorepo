# Rich Library Quick Start Guide

**Version**: MVP (Terminal Detection & Capability-Aware Rendering)

## Overview

The Rich library provides beautiful terminal output for Haskell with automatic terminal capability detection and graceful color degradation.

**New in this release:**
- ✅ Automatic terminal capability detection (TrueColor, Color256, Color16, NoColor)
- ✅ Graceful color degradation based on terminal support
- ✅ Terminal size detection for responsive rendering
- ✅ Unicode width calculation (CJK, emoji, combining characters)

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
        "//haskell/libs/rich",
    ],
)
```

### Import in Your Code

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Rich  -- Everything you need!
```

---

## Quick Examples

### 1. Basic Styled Text (Auto-Detecting Capabilities)

```haskell
import Rich

main :: IO () = do
  console <- newConsole  -- Automatically detects terminal capabilities!

  -- Colors adapt automatically:
  -- - TrueColor terminal: Full RGB colors
  -- - Color16 terminal: Nearest basic color
  -- - NoColor: Plain text (keeps bold/italic/etc)
  printStyledLn console (bold . red $ style) "Error: Something went wrong!"
  printStyledLn console (green $ style) "Success!"
```

### 2. RGB Colors with Automatic Degradation

```haskell
import Rich

main :: IO () = do
  console <- newConsole

  -- RGB color automatically adapts to terminal:
  printStyledLn console (rgb 100 150 200 style) "Beautiful blue text"
  -- TrueColor term: Shows exact RGB(100, 150, 200)
  -- Color16 term: Degrades to basic blue
  -- NoColor: Shows as plain text
```

### 3. Combining Styles

```haskell
import Rich

main :: IO () = do
  console <- newConsole

  printStyledLn console (bold . italic . underline . red $ style)
    "Bold, italic, underlined red text"

  printStyledLn console (dim . cyan $ style) "Dim cyan text"
  printStyledLn console (strikethrough . yellow $ style) "Strikethrough yellow"
```

### 4. Convenience Messages

```haskell
import Rich

main :: IO () = do
  console <- newConsole

  printSuccess console "Operation completed successfully!"
  printError console "An error occurred"
  printWarning console "This is a warning"
  printInfo console "Informational message"
```

### 5. Terminal Size Detection

```haskell
import Rich

main :: IO () = do
  -- Get terminal size (returns Nothing if output redirected)
  maybeSize <- getTerminalSize
  case maybeSize of
    Just size ->
      putStrLn $ "Terminal: " ++ show (termWidth size) ++ " cols × "
                              ++ show (termHeight size) ++ " rows"
    Nothing ->
      putStrLn "Not in a terminal"

  -- Or use helpers with fallback defaults:
  width <- getTerminalWidth   -- Returns 80 if not a terminal
  height <- getTerminalHeight -- Returns 24 if not a terminal
```

### 6. Unicode Width Calculation

```haskell
import Rich
import qualified Data.Text as T

main :: IO () = do
  let ascii = "Hello"
      cjk = "你好"
      emoji = "😀🎉"

  putStrLn $ "ASCII: '" ++ T.unpack ascii ++ "' = "
          ++ show (displayWidth ascii) ++ " columns"  -- 5
  putStrLn $ "CJK:   '" ++ T.unpack cjk ++ "' = "
          ++ show (displayWidth cjk) ++ " columns"    -- 4 (2 chars × 2)
  putStrLn $ "Emoji: '" ++ T.unpack emoji ++ "' = "
          ++ show (displayWidth emoji) ++ " columns"  -- 4 (2 emoji × 2)
```

### 7. Manual Capability Detection

```haskell
import Rich

main :: IO () = do
  capability <- detectColorCapability

  case capability of
    TrueColor -> putStrLn "Your terminal supports full RGB!"
    Color256  -> putStrLn "Your terminal supports 256 colors"
    Color16   -> putStrLn "Your terminal supports basic 16 colors"
    NoColor   -> putStrLn "No color support detected"

  -- Use capability-aware rendering manually if needed:
  let styled = renderTextWithCapability capability (rgb 100 150 200 style) "Text"
  putStrLn styled
```

---

## Color Palette

### Basic Colors

```haskell
black, red, green, yellow, blue, magenta, cyan, white :: Style -> Style
```

### Bright Colors

```haskell
brightBlack, brightRed, brightGreen, brightYellow :: Style -> Style
brightBlue, brightMagenta, brightCyan, brightWhite :: Style -> Style
```

### RGB Colors (Automatic Degradation)

```haskell
rgb :: Word8 -> Word8 -> Word8 -> Style -> Style

-- Example:
printStyledLn console (rgb 255 128 64 style) "Orange text"
```

### Background Colors

```haskell
onBlack, onRed, onGreen, onYellow :: Style -> Style
onBlue, onMagenta, onCyan, onWhite :: Style -> Style
onBrightBlack, onBrightRed, onBrightGreen, onBrightYellow :: Style -> Style
onBrightBlue, onBrightMagenta, onBrightCyan, onBrightWhite :: Style -> Style
onRgb :: Word8 -> Word8 -> Word8 -> Style -> Style

-- Example:
printStyledLn console (red . onWhite $ style) "Red on white"
```

---

## Text Attributes

```haskell
bold, italic, underline, dim :: Style -> Style
blink, reverse, hidden, strikethrough :: Style -> Style

-- Example:
printStyledLn console (bold . underline $ style) "Bold and underlined"
```

---

## Existing Features

The Rich library also includes (pre-existing functionality):

### Tables
```haskell
let table = simpleTable
      ["Name", "Age", "City"]
      [ ["Alice", "30", "New York"]
      , ["Bob", "25", "London"]
      ]
putStrLn $ T.unpack $ renderTable table
```

### Panels
```haskell
let panel = simplePanel "Welcome" "This is a panel with a title!"
putStrLn $ T.unpack $ renderPanel panel
```

### Trees
```haskell
let tree = addChild (leaf "child1")
         $ addChild (leaf "child2")
         $ tree "root"
putStrLn $ T.unpack $ renderTree tree
```

### Progress Bars
```haskell
let progress = setCurrent 50
             $ setDescription "Processing"
             $ progressBar 100
putStrLn $ T.unpack $ renderProgressWithPercentage progress
```

---

## Best Practices

### ✅ DO: Use Console for Styled Output

```haskell
main :: IO () = do
  console <- newConsole  -- Detects capabilities once
  printStyledLn console (red $ style) "Error message"
  printStyledLn console (green $ style) "Success message"
```

**Why**: Console automatically detects terminal capabilities once and applies them to all output.

### ✅ DO: Use displayWidth for Text Alignment

```haskell
import qualified Data.Text as T

alignCenter :: Int -> Text -> Text
alignCenter targetWidth text =
  let actualWidth = displayWidth text
      padding = max 0 (targetWidth - actualWidth)
      leftPad = padding `div` 2
      rightPad = padding - leftPad
  in T.replicate leftPad " " <> text <> T.replicate rightPad " "
```

**Why**: Handles Unicode characters correctly (emoji, CJK).

### ❌ DON'T: Use T.length for Visual Width

```haskell
-- Wrong! T.length counts characters, not visual width
T.length "😀" == 1  -- But displays as 2 columns wide!

-- Correct! displayWidth counts visual columns
displayWidth "😀" == 2  -- Accurate for terminal display
```

### ✅ DO: Check Terminal Size for Responsive Output

```haskell
main :: IO () = do
  width <- getTerminalWidth

  if width < 80
    then putStrLn "Narrow terminal: showing compact view"
    else putStrLn "Wide terminal: showing detailed view"
```

---

## Testing

```bash
# Run tests
bazel test //haskell/libs/rich/test:rich-test --test_output=all

# Run demo (shows all features)
bazel run //haskell/app/rich-demo:rich-demo

# Build library
bazel build //haskell/libs/rich
```

---

## API Reference

### Module Structure

```haskell
-- Main module (re-exports everything)
import Rich

-- Or import specific modules:
import Rich.Style                -- Style types and rendering
import Rich.Console              -- Console output
import Rich.Terminal             -- All terminal detection (re-exports below)
import Rich.Terminal.Size        -- Terminal dimensions
import Rich.Terminal.Width       -- Unicode width calculation
import Rich.Terminal.Capability  -- Color capability detection
import Rich.Table                -- Table rendering
import Rich.Panel                -- Panel rendering
import Rich.Tree                 -- Tree rendering
import Rich.Progress             -- Progress bars
```

### Key Functions

**Terminal Detection:**
```haskell
getTerminalSize       :: IO (Maybe TerminalSize)
getTerminalWidth      :: IO Int
getTerminalHeight     :: IO Int
detectColorCapability :: IO ColorCapability
displayWidth          :: Text -> Int
```

**Console Output:**
```haskell
newConsole      :: IO Console
printStyledLn   :: Console -> Style -> Text -> IO ()
printSuccess    :: Console -> Text -> IO ()
printError      :: Console -> Text -> IO ()
printWarning    :: Console -> Text -> IO ()
printInfo       :: Console -> Text -> IO ()
```

**Styling:**
```haskell
style :: Style  -- Base style
bold, italic, underline :: Style -> Style
red, green, blue :: Style -> Style
rgb :: Word8 -> Word8 -> Word8 -> Style -> Style
renderTextWithCapability :: ColorCapability -> Style -> Text -> Text
```

---

## Complete Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Rich
import qualified Data.Text.IO as TIO

main :: IO () = do
  console <- newConsole

  -- Header
  printStyledLn console (bold . brightCyan $ style) "╔════════════════╗"
  printStyledLn console (bold . brightCyan $ style) "║  My Cool App   ║"
  printStyledLn console (bold . brightCyan $ style) "╚════════════════╝"
  emptyLine console

  -- Show terminal info
  capability <- detectColorCapability
  width <- getTerminalWidth
  printInfo console $ "Terminal: " <> T.pack (show width)
                   <> " cols, " <> T.pack (show capability)
  emptyLine console

  -- Do some work
  printInfo console "Starting process..."
  printSuccess console "Step 1 completed"
  printSuccess console "Step 2 completed"
  printSuccess console "All done!"
  emptyLine console

  -- Table
  let table = simpleTable
        ["Task", "Status"]
        [ ["Initialize", "✓ Done"]
        , ["Process", "✓ Done"]
        , ["Finalize", "✓ Done"]
        ]
  TIO.putStrLn (renderTable table)
```

---

## What's Next?

This is the MVP release focusing on terminal detection and capability-aware rendering.

**Upcoming features** (not yet implemented):
- Multi-line table cells with automatic row height
- Responsive table rendering (auto-fit to terminal width)
- Responsive panel rendering
- Enhanced Unicode handling for trees and progress bars

**Current implementation status:** 20/69 tasks complete (MVP scope)

For the full feature roadmap, see `specs/003-haskell-rich-library/spec.md`

---

## Troubleshooting

**Q: Colors don't appear in my terminal**
- Check: `detectColorCapability` - it may return `NoColor`
- Solution: Ensure `TERM` environment variable is set (e.g., `TERM=xterm-256color`)

**Q: Terminal size returns Nothing**
- Check: Are you redirecting output? (`./app > file.txt`)
- Solution: `getTerminalWidth` provides fallback (80 columns)

**Q: Unicode characters misaligned**
- Check: Are you using `T.length` instead of `displayWidth`?
- Solution: Use `displayWidth` for visual column calculation

**Q: RGB colors look wrong**
- Check: Your terminal may only support Color16
- Solution: Colors automatically degrade - this is expected behavior

---

## License

MIT

## Contributing

See repository for contribution guidelines.
