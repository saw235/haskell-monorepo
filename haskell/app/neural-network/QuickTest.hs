module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Random (mkStdGen)

-- Import the specific architecture test from the main neural network
main :: IO ()
main = do
  putStrLn "Testing specific [2,8,6,4,1] architecture that crashes..."
  
  -- Try to reproduce the exact crash
  putStrLn "This should crash with 'index out of bounds (1,1)'"
  
  -- We'll need to compile and link with the actual Main.hs to test this
  putStrLn "Run: bazel run //haskell/app/neural-network:neural-network"
  putStrLn "It should crash during training with the [2,8,6,4,1] architecture"