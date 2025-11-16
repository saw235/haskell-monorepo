{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Rich
-- Description : Beautiful terminal output for Haskell
-- Copyright   : (c) 2024
-- License     : MIT
-- Maintainer  : example@example.com
-- Stability   : experimental
--
-- Rich is a Haskell library for rich text and beautiful formatting in the terminal,
-- inspired by Python's Rich library.
--
-- = Features
--
-- * Text styling with colors, bold, italic, underline, and more
-- * Tables with various border styles and alignments
-- * Panels and boxes for highlighting content
-- * Tree structures for hierarchical data
-- * Progress bars with customizable styles
-- * Console output with automatic color support
--
-- = Quick Start
--
-- == Styled Text
--
-- @
-- import Rich
--
-- main :: IO () = do
--   console <- newConsole
--   printStyledLn console (bold . red $ style) "Error: Something went wrong!"
--   printStyledLn console (green $ style) "Success!"
-- @
--
-- == Tables
--
-- @
-- import Rich
--
-- main :: IO () = do
--   let table = simpleTable
--         ["Name", "Age", "City"]
--         [ ["Alice", "30", "New York"]
--         , ["Bob", "25", "London"]
--         , ["Charlie", "35", "Paris"]
--         ]
--   putStrLn $ T.unpack $ renderTable table
-- @
--
-- == Panels
--
-- @
-- import Rich
--
-- main :: IO () = do
--   let panel = simplePanel "Welcome" "This is a beautiful panel!"
--   putStrLn $ T.unpack $ renderPanel panel
-- @
--
-- == Progress Bars
--
-- @
-- import Rich
-- import Control.Concurrent (threadDelay)
--
-- main :: IO () = do
--   let total = 100
--   mapM_ (\\i -> do
--     let progress = setCurrent (fromIntegral i) $ setDescription "Processing" $ progressBar total
--     putStr "\\r"
--     putStr $ T.unpack $ renderProgressWithPercentage progress
--     threadDelay 50000
--     ) [0..total]
--   putStrLn ""
-- @
module Rich
  ( -- * Re-exports from Rich.Style
    module Rich.Style,
    -- * Re-exports from Rich.Console
    module Rich.Console,
    -- * Re-exports from Rich.Table
    module Rich.Table,
    -- * Re-exports from Rich.Panel
    module Rich.Panel,
    -- * Re-exports from Rich.Tree
    module Rich.Tree,
    -- * Re-exports from Rich.Progress
    module Rich.Progress,
    -- * Re-exports from Rich.Terminal
    module Rich.Terminal,
  )
where

import Rich.Console
import Rich.Panel hiding (setBorderStyle, setStyle, setTitle)
import Rich.Progress hiding (setStyle)
import Rich.Style
import Rich.Table hiding (setBorderStyle, setTitle)
import Rich.Terminal
import Rich.Tree hiding (setStyle)
