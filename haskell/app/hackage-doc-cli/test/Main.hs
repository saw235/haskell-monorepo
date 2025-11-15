module Main (main) where

import CacheSpec (cacheTests)
import ParserSpec (parserTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import TreeDisplaySpec (treeDisplayTests)

main :: IO ()
main = do
  counts <- runTestTT $ TestList [parserTests, cacheTests, treeDisplayTests]
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
