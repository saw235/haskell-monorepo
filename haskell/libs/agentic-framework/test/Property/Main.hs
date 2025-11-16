module Main (main) where

import qualified Property.AgentProps as AgentProps
import qualified Property.SummarizationProps as SummarizationProps
import qualified Property.TokenizerProps as TokenizerProps
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Agent" AgentProps.spec
  describe "Summarization" SummarizationProps.spec
  describe "Tokenizer" TokenizerProps.spec
