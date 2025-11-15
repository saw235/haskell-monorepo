module Main (main) where

import CacheSpec (cacheTests)
import ParserSpec (parserTests)
import TreeDisplaySpec (treeDisplayTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  counts <- runTestTT $ TestList [parserTests, cacheTests, treeDisplayTests]
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
