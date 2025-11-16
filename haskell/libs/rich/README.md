# Rich - Beautiful Terminal Output for Haskell

Rich is a Haskell library for creating beautiful, formatted terminal output. Inspired by Python's [Rich](https://github.com/Textualize/rich) library, it provides an easy-to-use API for styling text, creating tables, panels, trees, and progress bars.

## Features

### Core Features
- **Styled Text**: Colors (16 standard + RGB), bold, italic, underline, strikethrough, and more
- **Tables**: Beautiful tables with multiple border styles and automatic column sizing
- **Panels**: Highlight content with bordered panels and boxes
- **Trees**: Display hierarchical data with tree structures
- **Progress Bars**: Animated progress bars with customizable styles
- **Console Output**: Convenient console interface with success/error/warning/info messages

### New: Terminal Detection & Smart Rendering ✨
- **🎨 Auto Color Degradation**: RGB colors automatically adapt to terminal capabilities (TrueColor → Color256 → Color16 → NoColor)
- **📐 Terminal Size Detection**: Query terminal dimensions for responsive rendering
- **🌍 Unicode Width Calculation**: Correct handling of emoji, CJK characters, and combining characters
- **🔍 Capability Detection**: Automatic detection of terminal color support

**[📖 See QUICKSTART.md for detailed examples](./QUICKSTART.md)**

## Quick Start

### Basic Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Rich

main :: IO () = do
  console <- newConsole

  -- Styled text
  printStyledLn console (bold . red $ style) "Error: Something went wrong!"
  printStyledLn console (green $ style) "Success!"

  -- Console messages
  printSuccess console "Operation completed"
  printError console "Failed to connect"
  printWarning console "Low disk space"
  printInfo console "Server started on port 8080"
```

### Tables

```haskell
import Rich
import qualified Data.Text.IO as TIO

main :: IO () = do
  let table = simpleTable
        ["Name", "Age", "City"]
        [ ["Alice", "30", "New York"]
        , ["Bob", "25", "London"]
        , ["Charlie", "35", "Paris"]
        ]

  TIO.putStrLn (renderTable table)
```

Output:
```
┌─────────┬─────┬──────────┐
│ Name    │ Age │ City     │
├─────────┼─────┼──────────┤
│ Alice   │ 30  │ New York │
│ Bob     │ 25  │ London   │
│ Charlie │ 35  │ Paris    │
└─────────┴─────┴──────────┘
```

### Panels

```haskell
import Rich
import qualified Data.Text.IO as TIO

main :: IO () = do
  let panel = simplePanel "Welcome" "This is a beautiful panel!"
  TIO.putStrLn (renderPanel panel)
```

Output:
```
╭─ Welcome ──────────────────────╮
│                                │
│  This is a beautiful panel!   │
│                                │
╰────────────────────────────────╯
```

### Trees

```haskell
import Rich
import qualified Data.Text.IO as TIO

main :: IO () = do
  let fileTree =
        addChild (leaf "README.md") $
        addChild (addChild (leaf "Main.hs") $ tree "src") $
        tree "project"

  TIO.putStrLn (renderTree fileTree)
```

Output:
```
project
├── src
│   └── Main.hs
└── README.md
```

### Progress Bars

```haskell
import Rich
import qualified Data.Text.IO as TIO
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

main :: IO () = do
  let total = 100 :: Double
  forM_ [0..round total] $ \i -> do
    let progress = setCurrent (fromIntegral i) $
                   setDescription "Processing" $
                   progressBar total
    putStr "\r"
    TIO.putStr (renderProgressWithPercentage progress)
    threadDelay 50000
  putStrLn ""
```

## API Documentation

### Rich.Style

Text styling with colors and attributes.

#### Colors

**Standard Colors**:
- `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`

**Bright Colors**:
- `brightBlack`, `brightRed`, `brightGreen`, `brightYellow`, `brightBlue`, `brightMagenta`, `brightCyan`, `brightWhite`

**RGB Colors**:
```haskell
rgb 255 100 50 style  -- Custom RGB color
```

**Background Colors**:
All colors have `on` variants for backgrounds:
```haskell
onRed style           -- Red background
onBrightBlue style    -- Bright blue background
onRgb 255 100 50 style -- RGB background
```

#### Style Attributes

- `bold` - Bold text
- `italic` - Italic text
- `underline` - Underlined text
- `dim` - Dimmed text
- `blink` - Blinking text
- `reverse` - Reversed foreground/background
- `hidden` - Hidden text
- `strikethrough` - Strikethrough text

#### Combining Styles

Styles can be combined using function composition:

```haskell
bold . red $ style                    -- Bold red
italic . underline . blue $ style     -- Italic underlined blue
bold . red . onWhite $ style          -- Bold red on white background
```

### Rich.Table

Create formatted tables with various border styles.

#### Creating Tables

```haskell
-- Simple table from headers and data
simpleTable :: [Text] -> [[Text]] -> Table

-- Custom table with manual column configuration
table :: Table
addColumn :: Column -> Table -> Table
addRow :: Row -> Table -> Table
```

#### Border Styles

- `NoBorder` - No borders
- `SimpleBorder` - Simple line borders (┌─┐)
- `RoundedBorder` - Rounded corners (╭─╮)
- `DoubleBorder` - Double line borders (╔═╗)
- `HeavyBorder` - Heavy line borders (┏━┓)

```haskell
setBorderStyle DoubleBorder table
```

#### Column Alignment

```haskell
Column "Name" AlignLeft Nothing Nothing
Column "Price" AlignRight Nothing Nothing
Column "Status" AlignCenter Nothing Nothing
```

### Rich.Panel

Create bordered panels for highlighting content.

```haskell
-- Simple panel with title and content
simplePanel :: Text -> Text -> Panel

-- Customization
setTitle :: Text -> Panel -> Panel
setSubtitle :: Text -> Panel -> Panel
setPadding :: Padding -> Panel -> Panel
setBorderStyle :: BorderStyle -> Panel -> Panel
```

### Rich.Tree

Display hierarchical data as trees.

```haskell
-- Create a tree node
tree :: Text -> Tree

-- Create a leaf node
leaf :: Text -> Tree

-- Add children
addChild :: Tree -> Tree -> Tree

-- Example
let myTree = addChild (leaf "child1") $
             addChild (leaf "child2") $
             tree "parent"
```

### Rich.Progress

Animated progress bars.

```haskell
-- Create a progress bar
progressBar :: Double -> ProgressBar

-- Set values
setCurrent :: Double -> ProgressBar -> ProgressBar
setDescription :: Text -> ProgressBar -> ProgressBar
setWidth :: Int -> ProgressBar -> ProgressBar

-- Render
renderProgress :: ProgressBar -> Text
renderProgressWithPercentage :: ProgressBar -> Text
```

### Rich.Console

Convenient console output functions.

```haskell
-- Create console
newConsole :: IO Console

-- Print functions
print :: Console -> Text -> IO ()
printLn :: Console -> Text -> IO ()
printStyled :: Console -> Style -> Text -> IO ()
printStyledLn :: Console -> Style -> Text -> IO ()

-- Specialized messages
printSuccess :: Console -> Text -> IO ()  -- Green with ✓
printError :: Console -> Text -> IO ()    -- Red with ✗
printWarning :: Console -> Text -> IO ()  -- Yellow with ⚠
printInfo :: Console -> Text -> IO ()     -- Blue with ℹ

-- Utilities
rule :: Console -> Maybe Text -> IO ()    -- Horizontal rule
emptyLine :: Console -> IO ()             -- Print empty line
```

## Building with Bazel

Add the library to your `BUILD.bazel` dependencies:

```bazel
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

## Running the Demo

A comprehensive demo application is included that showcases all features:

```bash
bazel run //haskell/app/rich-demo:rich-demo
```

The demo displays:
- All color options (standard and bright colors)
- Text styling (bold, italic, underline, etc.)
- Console messages (success, error, warning, info)
- Tables with different border styles
- Panels with various configurations
- Tree structures
- Animated progress bars
- Combined features

## Design Philosophy

Rich follows these design principles:

1. **Composable**: Style functions can be easily combined
2. **Type-Safe**: Strong typing prevents common errors
3. **Idiomatic**: Uses standard Haskell patterns and conventions
4. **Minimal Dependencies**: Only depends on `base` and `text`
5. **Beautiful Default**: Sensible defaults that look great out of the box

## Examples

### Error Message with Panel

```haskell
let errorPanel = setBorderStyle DoubleBorder $
                 simplePanel "Error" "Connection timeout after 30 seconds"
TIO.putStrLn (renderPanel errorPanel)
```

### Styled Table

```haskell
let styledTable = setBorderStyle RoundedBorder $
                  setTitle "Sales Report" $
                  simpleTable
                    ["Product", "Quantity", "Revenue"]
                    [ ["Widget A", "150", "$3,750"]
                    , ["Widget B", "200", "$8,000"]
                    , ["Widget C", "75", "$2,250"]
                    ]
TIO.putStrLn (renderTable styledTable)
```

### Multiple Progress Bars

```haskell
let tasks = [("Downloading", 100), ("Installing", 50), ("Building", 80)]
forM_ tasks $ \(desc, total) -> do
  forM_ [0..round total] $ \i -> do
    let prog = setCurrent (fromIntegral i) $
               setDescription desc $
               progressBar total
    putStr "\r"
    TIO.putStr (renderProgressWithPercentage prog)
    threadDelay 10000
  putStrLn ""
```

## Comparison with Python's Rich

| Feature | Python Rich | Haskell Rich |
|---------|-------------|--------------|
| Styled Text | ✓ | ✓ |
| Tables | ✓ | ✓ |
| Panels | ✓ | ✓ |
| Trees | ✓ | ✓ |
| Progress Bars | ✓ | ✓ |
| Syntax Highlighting | ✓ | ✗ (planned) |
| Markdown Rendering | ✓ | ✗ (planned) |
| Live Display | ✓ | ✗ (planned) |

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Credits

Inspired by [Textualize/rich](https://github.com/Textualize/rich) by Will McGugan.
