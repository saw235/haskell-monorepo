module Main where

import CLI.Commands (runCommand)
import CLI.Options (parseOptions)

main :: IO ()
main = do
  opts <- parseOptions
  runCommand opts
