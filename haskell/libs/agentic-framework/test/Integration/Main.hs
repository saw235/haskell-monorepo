module Main (main) where

import qualified Integration.ContextManagementSpec
import qualified Integration.SimpleAgentSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Simple Agent" Integration.SimpleAgentSpec.spec
  describe "Context Management" Integration.ContextManagementSpec.spec
