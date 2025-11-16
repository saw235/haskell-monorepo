{-# LANGUAGE CPP #-}

-- |
-- Module      : Rich.Terminal.Size
-- Description : Query terminal dimensions for responsive rendering
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functions to detect the current terminal size (width and height).
-- It wraps the terminal-size library to provide a consistent interface for querying
-- terminal dimensions across different platforms (Linux, macOS, Windows).
module Rich.Terminal.Size
  ( -- * Types
    TerminalSize (..),

    -- * Queries
    getTerminalSize,
    getTerminalWidth,
    getTerminalHeight,
  )
where

import qualified System.Console.Terminal.Size as TS
import System.IO (Handle, stdout)

-- | Represents terminal dimensions in columns and rows
data TerminalSize = TerminalSize
  { -- | Terminal width in columns (positive integer)
    termWidth :: Int,
    -- | Terminal height in rows (positive integer)
    termHeight :: Int
  }
  deriving (Show, Eq)

-- | Get the current terminal size for stdout
--
-- Returns 'Just TerminalSize' if stdout is a terminal, 'Nothing' otherwise.
-- This is useful for responsive rendering that adapts to the terminal dimensions.
--
-- Example:
-- @
-- main :: IO () = do
--   maybeSize <- getTerminalSize
--   case maybeSize of
--     Just (TerminalSize w h) -> putStrLn $ "Terminal: " ++ show w ++ "x" ++ show h
--     Nothing -> putStrLn "Not a terminal (output redirected)"
-- @
getTerminalSize :: IO (Maybe TerminalSize)
getTerminalSize = getTerminalSizeFor stdout

-- | Get terminal size for a specific handle
--
-- Internal helper that wraps terminal-size's size function
getTerminalSizeFor :: Handle -> IO (Maybe TerminalSize)
getTerminalSizeFor handle = do
  maybeSize <- TS.size
  return $ case maybeSize of
    Just (TS.Window h w) ->
      Just $ TerminalSize {termWidth = w, termHeight = h}
    Nothing -> Nothing

-- | Get the current terminal width, with fallback to 80 columns
--
-- Returns the actual terminal width if available, or 80 (standard default)
-- if not a terminal. This is useful when you need a width for rendering
-- but don't want to handle the Maybe case.
--
-- Example:
-- @
-- renderTable :: Table -> IO Text
-- renderTable table = do
--   width <- getTerminalWidth
--   return $ renderTableWithWidth table width
-- @
getTerminalWidth :: IO Int
getTerminalWidth = do
  maybeSize <- getTerminalSize
  return $ case maybeSize of
    Just size -> termWidth size
    Nothing -> 80 -- Standard default terminal width

-- | Get the current terminal height, with fallback to 24 rows
--
-- Returns the actual terminal height if available, or 24 (standard default)
-- if not a terminal.
--
-- Example:
-- @
-- renderList :: [Text] -> IO ()
-- renderList items = do
--   height <- getTerminalHeight
--   let visible = take (height - 2) items  -- Leave room for header/footer
--   mapM_ putStrLn visible
-- @
getTerminalHeight :: IO Int
getTerminalHeight = do
  maybeSize <- getTerminalSize
  return $ case maybeSize of
    Just size -> termHeight size
    Nothing -> 24 -- Standard default terminal height
