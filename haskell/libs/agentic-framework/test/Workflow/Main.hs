module Main where

import Test.Hspec
import qualified Workflow.CapabilitySpec
import qualified Workflow.DSLSpec

main :: IO ()
main = hspec $ do
  Workflow.CapabilitySpec.spec
  Workflow.DSLSpec.spec
