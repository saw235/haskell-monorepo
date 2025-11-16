module Main (main) where

import Test.Hspec
import qualified Integration.SimpleAgentSpec

main :: IO ()
main = hspec $ do
  describe "Integration Tests" $ do
    Integration.SimpleAgentSpec.spec
