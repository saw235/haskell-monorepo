module NeuralNetwork.Examples where

import Control.Monad.Reader (runReaderT)
import Data.Vector (Vector)
import qualified Data.Vector as V
import NeuralNetwork.Activation
import NeuralNetwork.Creation
import NeuralNetwork.Training
import NeuralNetwork.Types
import NeuralNetwork.Visualization
import System.Random (mkStdGen)

-- | XOR training examples
xorExamples :: [Example]
xorExamples =
  [ Example (V.fromList [0, 0]) (V.fromList [0]),
    Example (V.fromList [0, 1]) (V.fromList [1]),
    Example (V.fromList [1, 0]) (V.fromList [1]),
    Example (V.fromList [1, 1]) (V.fromList [0])
  ]

-- | Create a flexible neural network with different activations and initialization
createFlexibleNetwork :: IO NeuralNetwork
createFlexibleNetwork = do
  putStrLn "\n=== Flexible Neural Network Demo ==="
  putStrLn "Creating network with ReLU hidden layer and sigmoid output..."

  let gen = mkStdGen 123
      hiddenConfig =
        LayerConfig
          { layerSize = 4,
            layerActivation = reluActivation,
            layerInitStrategy = He 2 -- He initialization for ReLU
          }
      outputConfig =
        LayerConfig
          { layerSize = 1,
            layerActivation = sigmoidActivation,
            layerInitStrategy = Xavier 4 -- Xavier for sigmoid
          }
      configs = [hiddenConfig, outputConfig]
      (flexNetwork, _) = buildNetwork gen 2 configs

  -- Show initial flexible network structure using ReaderT
  runNetworkAnalysis flexNetwork

  -- Train the flexible network
  trainedFlexNetwork <- trainNetwork flexNetwork xorExamples 0.05 15000

  -- Test the flexible network
  putStrLn "\nTesting flexible network (ReLU + He init):"
  testNetwork trainedFlexNetwork xorExamples

  -- Show detailed structure after training using ReaderT
  putStrLn "\nFlexible network structure after training:"
  runNetworkViz printStructure trainedFlexNetwork

  return trainedFlexNetwork
