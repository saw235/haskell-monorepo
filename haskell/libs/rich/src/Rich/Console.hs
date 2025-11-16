{-# LANGUAGE OverloadedStrings #-}

module Rich.Console
  ( -- * Console Type
    Console,
    newConsole,

    -- * Printing Functions
    print,
    printLn,
    printStyled,
    printStyledLn,

    -- * Specialized Printing
    printSuccess,
    printError,
    printWarning,
    printInfo,

    -- * Line and Separator Functions
    rule,
    emptyLine,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rich.Style
import Rich.Terminal.Capability (ColorCapability, detectColorCapabilityFor)
import qualified System.IO as IO
import Prelude hiding (print, reverse)

-- | Console handle for output with color capability detection
data Console = Console
  { consoleHandle :: IO.Handle,
    consoleCapability :: ColorCapability
  }

-- | Create a new console using stdout with automatic capability detection
newConsole :: IO Console
newConsole = do
  capability <- detectColorCapabilityFor IO.stdout
  return $ Console IO.stdout capability

-- | Print text without newline
print :: Console -> Text -> IO ()
print console text = TIO.hPutStr (consoleHandle console) text

-- | Print text with newline
printLn :: Console -> Text -> IO ()
printLn console text = TIO.hPutStrLn (consoleHandle console) text

-- | Print styled text without newline (with automatic capability detection)
printStyled :: Console -> Style -> Text -> IO ()
printStyled console s text =
  TIO.hPutStr (consoleHandle console) (renderTextWithCapability (consoleCapability console) s text)

-- | Print styled text with newline (with automatic capability detection)
printStyledLn :: Console -> Style -> Text -> IO ()
printStyledLn console s text =
  TIO.hPutStrLn (consoleHandle console) (renderTextWithCapability (consoleCapability console) s text)

-- | Print success message (green with checkmark)
printSuccess :: Console -> Text -> IO ()
printSuccess console text =
  printStyledLn console (bold . green $ style) ("✓ " <> text)

-- | Print error message (red with cross)
printError :: Console -> Text -> IO ()
printError console text =
  printStyledLn console (bold . red $ style) ("✗ " <> text)

-- | Print warning message (yellow with warning sign)
printWarning :: Console -> Text -> IO ()
printWarning console text =
  printStyledLn console (bold . yellow $ style) ("⚠ " <> text)

-- | Print info message (blue with info sign)
printInfo :: Console -> Text -> IO ()
printInfo console text =
  printStyledLn console (bold . blue $ style) ("ℹ " <> text)

-- | Print a horizontal rule with optional title
rule :: Console -> Maybe Text -> IO ()
rule console maybeTitle = do
  let ruleChar = "─"
      width = 80
  case maybeTitle of
    Nothing ->
      printStyledLn console (dim $ style) (T.replicate width ruleChar)
    Just title ->
      let titleLen = T.length title
          padding = max 0 ((width - titleLen - 2) `div` 2)
          leftLine = T.replicate padding ruleChar
          rightLine = T.replicate (width - padding - titleLen - 2) ruleChar
       in printStyledLn console (dim $ style) (leftLine <> " " <> title <> " " <> rightLine)

-- | Print an empty line
emptyLine :: Console -> IO ()
emptyLine console = printLn console ""
