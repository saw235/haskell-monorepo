module Main (main) where

import CLI.Commands (handleCommand)
import CLI.Options (parseOptions, optCommand)

-- | Main entry point (T032)
main :: IO ()
main = do
  options <- parseOptions
  handleCommand (optCommand options)
