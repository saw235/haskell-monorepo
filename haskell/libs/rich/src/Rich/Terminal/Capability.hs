{-# LANGUAGE CPP #-}

-- |
-- Module      : Rich.Terminal.Capability
-- Description : Detect terminal color support
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functions to detect the color capabilities of the terminal.
-- It uses the ansi-terminal library to query terminal support and provides
-- a simple ColorCapability type for graceful degradation of color output.
module Rich.Terminal.Capability
  ( -- * Types
    ColorCapability (..),

    -- * Detection
    detectColorCapability,
    detectColorCapabilityFor,
    supportsANSI,
  )
where

import System.Console.ANSI (hSupportsANSI, hSupportsANSIColor)
import System.IO (Handle, stdout)

-- | Represents the color capability level of a terminal
--
-- Ordered from least to most capable: NoColor < Color16 < Color256 < TrueColor
-- You can use Ord to check capabilities: @cap >= Color16@ means color is supported
data ColorCapability
  = -- | No ANSI support (TERM=dumb or not a terminal)
    NoColor
  | -- | Basic 16-color ANSI
    Color16
  | -- | Extended 256-color palette
    Color256
  | -- | Full RGB 24-bit color
    TrueColor
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Detect color capability for stdout
--
-- Detection heuristics:
-- - If 'hSupportsANSIColor' returns True → TrueColor (assumes modern terminal)
-- - If 'hSupportsANSI' returns True → Color16 (basic ANSI only)
-- - Otherwise → NoColor (not a terminal or TERM=dumb)
--
-- Example:
-- @
-- printWithColor :: Text -> IO ()
-- printWithColor text = do
--   capability <- detectColorCapability
--   let styled = case capability of
--         TrueColor -> rgb 100 150 200 style
--         Color256 -> rgb 100 150 200 style  -- Use RGB for 256-color too
--         Color16 -> blue style
--         NoColor -> style
--   putStrLn $ renderText styled text
-- @
detectColorCapability :: IO ColorCapability
detectColorCapability = detectColorCapabilityFor stdout

-- | Detect color capability for a specific handle
--
-- Use this to detect capability for stderr separately from stdout, since
-- they may be redirected independently.
--
-- Example:
-- @
-- printError :: Text -> IO ()
-- printError msg = do
--   stderrCap <- detectColorCapabilityFor stderr
--   let color = if stderrCap >= Color16 then red style else style
--   hPutStrLn stderr $ renderText color msg
-- @
detectColorCapabilityFor :: Handle -> IO ColorCapability
detectColorCapabilityFor handle = do
  hasColor <- hSupportsANSIColor handle
  hasANSI <- hSupportsANSI handle
  return $ case (hasColor, hasANSI) of
    -- If color support detected, assume TrueColor (modern terminals)
    -- Note: ansi-terminal doesn't distinguish between Color256 and TrueColor,
    -- so we use TrueColor as the optimistic default for color-capable terminals
    (True, _) -> TrueColor
    -- If basic ANSI support but not color, use Color16
    (False, True) -> Color16
    -- No support at all
    (False, False) -> NoColor

-- | Check if the terminal supports ANSI escape codes
--
-- Returns True if stdout is a terminal that supports ANSI codes,
-- False otherwise (e.g., output redirected, TERM=dumb).
--
-- This is a simple yes/no check, useful for deciding whether to use
-- any ANSI formatting at all.
--
-- Example:
-- @
-- printStyled :: Text -> IO ()
-- printStyled text = do
--   hasANSI <- supportsANSI
--   if hasANSI
--     then putStrLn $ renderText (bold . blue $ style) text
--     else putStrLn $ T.unpack text  -- Plain output
-- @
supportsANSI :: IO Bool
supportsANSI = hSupportsANSI stdout
