module Main where

import CLI.Options (parseOptions)
import CLI.Commands (runCommand)

main :: IO ()
main = do
  opts <- parseOptions
  runCommand opts