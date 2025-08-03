module NeuralNetwork.Backward where

import NeuralNetwork.Types
import NeuralNetwork.Forward (computeActivations)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Backward pass to compute gradients
backward :: NeuralNetwork -> Vector Double -> Vector Double -> 
           ([Vector (Vector Double)], [Vector Double])
backward network inputs expectedOutputs = 
  let activations = computeActivations network inputs
      deltas = computeDeltas network activations expectedOutputs
      weightGradients = computeWeightGradients network activations deltas
      biasGradients = computeBiasGradients deltas
  in (weightGradients, biasGradients)

-- | Compute deltas for backpropagation
computeDeltas :: NeuralNetwork -> [Vector Double] -> Vector Double -> [Vector Double]
computeDeltas NeuralNetwork{..} activations expectedOutputs = 
  let outputLayer = last layers
      outputActivation = last activations
      outputDelta = V.zipWith (\act exp -> 
        (act - exp) * derivative (activation outputLayer) act) outputActivation expectedOutputs
      
      hiddenDeltas = computeHiddenDeltas layers activations outputDelta
  in hiddenDeltas ++ [outputDelta]

-- | Compute deltas for hidden layers (working backwards from output)
computeHiddenDeltas :: [Layer] -> [Vector Double] -> Vector Double -> [Vector Double]
computeHiddenDeltas [] _ _ = []
computeHiddenDeltas [_] _ _ = []  -- No hidden layers case
computeHiddenDeltas layers activations outputDelta = 
  let reversedLayers = reverse (init layers)
      reversedActivations = reverse (init activations)
      forwardLayers = drop 1 layers  -- layers that propagate error backwards
  in reverse (computeHiddenDeltasBackward reversedLayers reversedActivations forwardLayers outputDelta)

-- | Compute hidden deltas working backwards
computeHiddenDeltasBackward :: [Layer] -> [Vector Double] -> [Layer] -> Vector Double -> [Vector Double]
computeHiddenDeltasBackward [] _ _ _ = []
computeHiddenDeltasBackward (currentLayer:restLayers) (currentActivation:restActivations) (nextLayer:restNextLayers) currentDelta =
  let layerDelta = V.imap (\i activationValue -> 
        let weightedSum = V.sum (V.imap (\j delta -> (weights nextLayer V.! j V.! i) * delta) currentDelta)
        in weightedSum * derivative (activation currentLayer) activationValue) currentActivation
      restDeltas = computeHiddenDeltasBackward restLayers restActivations restNextLayers layerDelta
  in layerDelta : restDeltas
computeHiddenDeltasBackward _ _ [] _ = []

-- | Compute weight gradients
computeWeightGradients :: NeuralNetwork -> [Vector Double] -> [Vector Double] -> 
                        [Vector (Vector Double)]
computeWeightGradients NeuralNetwork{..} activations deltas =
  zipWith3 computeLayerWeightGradients layers (init activations) deltas

-- | Compute weight gradients for a single layer
computeLayerWeightGradients :: Layer -> Vector Double -> Vector Double -> 
                             Vector (Vector Double)
computeLayerWeightGradients Layer{..} inputActivation delta =
  V.map (\neuronDelta -> 
    V.map (\input -> input * neuronDelta) inputActivation) delta

-- | Compute bias gradients
computeBiasGradients :: [Vector Double] -> [Vector Double]
computeBiasGradients = id