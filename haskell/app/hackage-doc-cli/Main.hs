module Main (main) where

import CLI.Commands (handleCommand)
import CLI.Options (optCommand, parseOptions)

-- | Main entry point (T032)
main :: IO ()
main = do
  options <- parseOptions
  handleCommand (optCommand options)
