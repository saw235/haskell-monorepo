module Main where

import Control.Monad.Reader (runReaderT)
import NeuralNetwork
import System.Random (mkStdGen)

main :: IO ()
main = do
  putStrLn "Building a simple neural network from scratch in Haskell!"
  putStrLn "Training on XOR problem..."

  -- Original network with all sigmoid activations
  putStrLn "\n=== Original Sigmoid Network ==="
  let gen = mkStdGen 42
      (network, _) = createNetworkSigmoid gen [2, 8, 6, 4, 1]

  -- Print initial network structure using ReaderT
  putStrLn "\nInitial network analysis:"
  runNetworkViz (printConnectivityViz >> printWeightStatsViz) network

  trainedNetwork <- trainNetwork network xorExamples 0.1 10000
  testNetwork trainedNetwork xorExamples

  -- Print trained network details using ReaderT
  putStrLn "\nTrained network details:"
  runNetworkViz printStructure trainedNetwork

  -- Demonstrate flexible architecture
  flexNetwork <- createFlexibleNetwork

  -- Compare the two networks using ReaderT
  compareNetworkSizes network flexNetwork

  putStrLn "\nTraining complete!"
