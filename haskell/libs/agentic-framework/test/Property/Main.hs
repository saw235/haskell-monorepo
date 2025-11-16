module Main (main) where

import qualified Property.TokenizerProps as TokenizerProps
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Tokenizer" TokenizerProps.spec
