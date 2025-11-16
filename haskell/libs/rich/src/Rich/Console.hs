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
import qualified System.IO as IO
import Prelude hiding (print, reverse)

-- | Console handle for output
data Console = Console
  { consoleHandle :: IO.Handle
  }

-- | Create a new console using stdout
newConsole :: IO Console
newConsole = return $ Console IO.stdout

-- | Print text without newline
print :: Console -> Text -> IO ()
print console text = TIO.hPutStr (consoleHandle console) text

-- | Print text with newline
printLn :: Console -> Text -> IO ()
printLn console text = TIO.hPutStrLn (consoleHandle console) text

-- | Print styled text without newline
printStyled :: Console -> Style -> Text -> IO ()
printStyled console s text =
  TIO.hPutStr (consoleHandle console) (renderText s text)

-- | Print styled text with newline
printStyledLn :: Console -> Style -> Text -> IO ()
printStyledLn console s text =
  TIO.hPutStrLn (consoleHandle console) (renderText s text)

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
