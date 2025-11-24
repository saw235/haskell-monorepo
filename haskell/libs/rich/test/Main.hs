module Main (main) where

import Rich.Terminal.CapabilitySpec (capabilityTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Running Rich library tests..."
  putStrLn "================================"
  counts <- runTestTT $ TestList [capabilityTests]
  putStrLn "================================"
  if errors counts + failures counts == 0
    then do
      putStrLn $ "All tests passed! (" ++ show (cases counts) ++ " test cases)"
      exitSuccess
    else do
      putStrLn $ "Tests failed: " ++ show (failures counts) ++ " failures, " ++ show (errors counts) ++ " errors"
      exitFailure
