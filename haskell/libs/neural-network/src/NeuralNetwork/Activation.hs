module NeuralNetwork.Activation where

import NeuralNetwork.Types
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Sigmoid activation function
sigmoidActivation :: ActivationFunction
sigmoidActivation = ActivationFunction
  { activate = \x -> 1.0 / (1.0 + exp (-x))
  , derivative = \x -> x * (1.0 - x)
  }

-- | ReLU activation function
reluActivation :: ActivationFunction
reluActivation = ActivationFunction
  { activate = \x -> max 0 x
  , derivative = \x -> if x > 0 then 1 else 0
  }

-- | Tanh activation function
tanhActivation :: ActivationFunction
tanhActivation = ActivationFunction
  { activate = tanh
  , derivative = \x -> 1.0 - x * x
  }

-- | Linear activation function (no activation)
linearActivation :: ActivationFunction
linearActivation = ActivationFunction
  { activate = id
  , derivative = const 1
  }

-- | Softmax activation function (for output layer)
softmaxActivation :: Vector Double -> Vector Double
softmaxActivation xs = 
  let maxX = V.maximum xs
      exps = V.map (\x -> exp (x - maxX)) xs
      sumExps = V.sum exps
  in V.map (/ sumExps) exps

-- | Legacy functions for backward compatibility
sigmoid :: Double -> Double
sigmoid = activate sigmoidActivation

sigmoidDerivative :: Double -> Double
sigmoidDerivative = derivative sigmoidActivation