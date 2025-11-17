module Main (main) where

import qualified AgenticFramework.AgentSpec as AgentSpec
import qualified AgenticFramework.ContextSpec as ContextSpec
import qualified AgenticFramework.LoggingSpec as LoggingSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Agent" AgentSpec.spec
  describe "Context" ContextSpec.spec
  describe "Logging" LoggingSpec.spec
