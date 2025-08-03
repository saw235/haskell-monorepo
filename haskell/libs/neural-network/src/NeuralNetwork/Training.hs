module NeuralNetwork.Training where

import NeuralNetwork.Types
import NeuralNetwork.Forward (forward)
import NeuralNetwork.Backward (backward)
import NeuralNetwork.Loss (mse)
import Control.Monad (foldM, when)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)

-- | Update network weights and biases using gradients
updateNetwork :: NeuralNetwork -> [Vector (Vector Double)] -> 
                [Vector Double] -> Double -> NeuralNetwork
updateNetwork NeuralNetwork{..} weightGradients biasGradients learningRate =
  NeuralNetwork (zipWith3 updateLayer layers weightGradients biasGradients)
  where
    updateLayer layer weightGrad layerBiasGrad =
      Layer
        { weights = V.zipWith (\neuronWeights neuronGrads ->
            V.zipWith (\w g -> w - learningRate * g) neuronWeights neuronGrads)
          (weights layer) weightGrad
        , biases = V.zipWith (\b g -> b - learningRate * g) 
          (biases layer) layerBiasGrad
        , activation = activation layer
        }

-- | Train the network on a single example
trainExample :: NeuralNetwork -> Example -> Double -> NeuralNetwork
trainExample network Example{..} learningRate =
  let (weightGradients, biasGradients) = backward network inputs expectedOutputs
  in updateNetwork network weightGradients biasGradients learningRate

-- | Train the network on multiple examples
trainNetwork :: NeuralNetwork -> [Example] -> Double -> Int -> IO NeuralNetwork
trainNetwork network examples learningRate epochs = do
  foldM (\net epoch -> do
    let trainedNet = foldl (\n example -> trainExample n example learningRate) net examples
        avgError = sum (map (\Example{..} -> 
          mse (forward trainedNet inputs) expectedOutputs) examples) / fromIntegral (length examples)
    
    when (epoch `mod` 1000 == 0) $ do
      printf "Epoch %d, Average Error: %.6f\n" epoch avgError
    
    return trainedNet) network [1..epochs]

-- | Test the network
testNetwork :: NeuralNetwork -> [Example] -> IO ()
testNetwork network examples = do
  putStrLn "\nTesting the trained network:"
  putStrLn "Input\t\tExpected\tOutput\t\tError"
  putStrLn "-----\t\t--------\t-----\t\t-----"
  
  mapM_ (\Example{..} -> do
    let output = forward network inputs
        error = mse output expectedOutputs
    printf "[%.0f, %.0f]\t\t%.0f\t\t%.4f\t\t%.6f\n" 
      (inputs V.! 0) (inputs V.! 1) 
      (expectedOutputs V.! 0) (output V.! 0) error) examples