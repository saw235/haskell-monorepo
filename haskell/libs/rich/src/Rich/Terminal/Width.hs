{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Rich.Terminal.Width
-- Description : Calculate display width of Unicode text
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functions to calculate the visual display width of Unicode text,
-- accounting for wide characters (CJK, emoji), combining characters, and ANSI escape codes.
-- It wraps the wcwidth library to handle complex Unicode width calculations.
module Rich.Terminal.Width
  ( displayWidth,
    safeDisplayWidth,
    displayWidthIgnoringANSI,
  )
where

import Data.Char (ord)
import Data.Char.WCWidth (wcwidth)
import Data.Text (Text)
import qualified Data.Text as T

-- | Calculate the visual display width of text
--
-- This function accounts for:
-- - ASCII characters: width 1
-- - CJK characters: width 2
-- - Emoji: width 2
-- - Combining characters: width 0
-- - Control characters: width 0 (treated as unprintable)
--
-- Examples:
-- @
-- displayWidth "hello"    == 5   -- Regular ASCII
-- displayWidth "你好"      == 4   -- 2 CJK chars × 2
-- displayWidth "😀"        == 2   -- Emoji (wide character)
-- displayWidth "e\x0301"  == 1   -- e + combining acute accent
-- displayWidth "\t"       == 0   -- Tab (control character)
-- @
--
-- Use this for correct text alignment in terminals.
displayWidth :: Text -> Int
displayWidth text =
  let str = T.unpack text
      -- Sum the widths of individual characters
      -- wcwidth returns -1 for unprintable/control chars, treat as 0
      width = sum $ map (\c -> max 0 (wcwidth c)) str
   in width

-- | Safe variant that treats unknown characters as width 0
--
-- This is identical to 'displayWidth' but more explicit about handling
-- unusual Unicode. The wcwidth library already handles edge cases safely,
-- so this is primarily for documentation purposes.
--
-- Use when processing untrusted input that may contain unusual Unicode.
safeDisplayWidth :: Text -> Int
safeDisplayWidth = displayWidth

-- | Calculate display width while ignoring ANSI escape sequences
--
-- This function strips all ANSI escape codes before calculating width,
-- so that formatting codes don't contribute to the visible width.
--
-- Examples:
-- @
-- displayWidthIgnoringANSI "\ESC[31mRed Text\ESC[0m" == 8  -- Only "Red Text" counts
-- displayWidthIgnoringANSI "Hello \ESC[1mWorld\ESC[0m" == 11  -- "Hello World"
-- @
--
-- Use this when measuring styled text that will be rendered with ANSI codes.
displayWidthIgnoringANSI :: Text -> Int
displayWidthIgnoringANSI text =
  let stripped = stripANSI text
   in displayWidth stripped

-- | Strip ANSI escape sequences from text
--
-- Removes all ANSI CSI (Control Sequence Introducer) codes of the form:
-- ESC [ <params> <command>
--
-- Examples:
-- @
-- stripANSI "\ESC[31mRed\ESC[0m" == "Red"
-- stripANSI "No \ESC[1mformatting\ESC[0m here" == "No formatting here"
-- @
stripANSI :: Text -> Text
stripANSI text = T.pack $ go (T.unpack text)
  where
    go [] = []
    go ('\ESC' : '[' : rest) = skipANSI rest
    go (c : rest) = c : go rest

    -- Skip ANSI sequence until we find the command character (letter)
    skipANSI [] = []
    skipANSI (c : rest)
      | isANSICommand c = go rest
      | otherwise = skipANSI rest

    -- ANSI commands are typically letters (A-Z, a-z) or @ character
    isANSICommand c =
      let code = ord c
       in (code >= 64 && code <= 90) || (code >= 97 && code <= 122) || code == 64
