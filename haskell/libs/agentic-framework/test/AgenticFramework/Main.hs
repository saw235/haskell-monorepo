module Main (main) where

import qualified AgenticFramework.AgentSpec as AgentSpec
import qualified AgenticFramework.LoggingSpec as LoggingSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Logging" LoggingSpec.spec
  describe "Agent" AgentSpec.spec
