module Main (main) where

import Test.Hspec
import qualified AgenticFramework.LoggingSpec as LoggingSpec
import qualified AgenticFramework.AgentSpec as AgentSpec

main :: IO ()
main = hspec $ do
  describe "Logging" LoggingSpec.spec
  describe "Agent" AgentSpec.spec
