module Main (main) where

import Test.Hspec
import qualified Property.TokenizerProps as TokenizerProps

main :: IO ()
main = hspec $ do
  describe "Tokenizer" TokenizerProps.spec
