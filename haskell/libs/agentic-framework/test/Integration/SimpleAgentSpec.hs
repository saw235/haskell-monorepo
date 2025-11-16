{-# LANGUAGE OverloadedStrings #-}

module Integration.SimpleAgentSpec (spec) where

import AgenticFramework.Agent
import AgenticFramework.Tool
import AgenticFramework.Types
import Test.Hspec

-- | Integration test for single agent with tools
spec :: Spec
spec = do
  describe "Single Agent with Tools" $ do
    it "agent completes task using calculator tool" $ pending

    it "agent completes task using file reader tool" $ pending

    it "agent uses multiple tools in sequence" $ pending
