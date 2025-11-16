-- |
-- Module      : Rich.Terminal
-- Description : Terminal detection and platform awareness
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides terminal detection functionality for responsive rendering.
-- It re-exports all terminal-related modules for convenient access.
--
-- = Terminal Size Detection
--
-- Query terminal dimensions for responsive rendering:
--
-- @
-- import Rich.Terminal
--
-- main :: IO () = do
--   width <- getTerminalWidth
--   putStrLn $ "Your terminal is " ++ show width ++ " columns wide"
-- @
--
-- = Unicode Width Calculation
--
-- Calculate visual display width accounting for wide characters:
--
-- @
-- import Rich.Terminal
--
-- -- Correctly handles CJK and emoji
-- let width = displayWidth "你好😀"  -- Returns 6 (not 3)
-- @
--
-- = Color Capability Detection
--
-- Detect terminal color support for graceful degradation:
--
-- @
-- import Rich.Terminal
-- import Rich.Style
--
-- main :: IO () = do
--   cap <- detectColorCapability
--   let styled = case cap of
--         TrueColor -> rgb 100 150 200 style
--         Color16 -> blue style
--         NoColor -> style
--   putStrLn $ renderText styled "Adaptive color!"
-- @
module Rich.Terminal
  ( -- * Size Detection
    module Rich.Terminal.Size,

    -- * Width Calculation
    module Rich.Terminal.Width,

    -- * Capability Detection
    module Rich.Terminal.Capability,
  )
where

import Rich.Terminal.Capability
import Rich.Terminal.Size
import Rich.Terminal.Width
