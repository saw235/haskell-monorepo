module NeuralNetwork.Forward where

import Data.Vector (Vector)
import qualified Data.Vector as V
import NeuralNetwork.Types

-- | Forward pass through a single layer
forwardLayer :: Layer -> Vector Double -> Vector Double
forwardLayer Layer {..} inputs =
  V.zipWith
    ( \neuronWeights bias ->
        activate activation (bias + V.sum (V.zipWith (*) neuronWeights inputs))
    )
    weights
    biases

-- | Compute activations for all layers
computeActivations :: NeuralNetwork -> Vector Double -> [Vector Double]
computeActivations NeuralNetwork {..} inputs =
  scanl (\activations layer -> forwardLayer layer activations) inputs layers

-- | Forward pass through the entire network
forward :: NeuralNetwork -> Vector Double -> Vector Double
forward network inputs = last (computeActivations network inputs)
