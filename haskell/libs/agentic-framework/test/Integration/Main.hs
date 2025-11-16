module Main (main) where

import qualified Integration.SimpleAgentSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Integration Tests" $ do
    Integration.SimpleAgentSpec.spec
