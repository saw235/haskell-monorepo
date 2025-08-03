{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NeuralNetwork.Types where

import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Neural network layer with weights, biases, and activation function
data Layer = Layer
  { weights :: Vector (Vector Double)  -- weights[i][j] = weight from neuron j to neuron i
  , biases :: Vector Double            -- bias for each neuron in this layer
  , activation :: ActivationFunction   -- activation function for this layer
  }

instance Show Layer where
  show (Layer w b _) = "Layer { weights = " ++ show w ++ ", biases = " ++ show b ++ ", activation = <function> }"

-- | Complete neural network
data NeuralNetwork = NeuralNetwork
  { layers :: [Layer]
  } deriving Show

-- | Training example
data Example = Example
  { inputs :: Vector Double
  , expectedOutputs :: Vector Double
  } deriving Show

-- | Weight initialization strategies
data InitStrategy = 
    Uniform Double Double    -- min max
  | Normal Double Double     -- mean stddev
  | Xavier Int               -- fan_in
  | He Int                   -- fan_in
  deriving Show

-- | Layer configuration
data LayerConfig = LayerConfig
  { layerSize :: Int
  , layerActivation :: ActivationFunction
  , layerInitStrategy :: InitStrategy
  }

-- | Optimizer types
data Optimizer = 
    SGD Double                           -- learning rate
  | Adam Double Double Double            -- learning rate, beta1, beta2
  | RMSprop Double Double               -- learning rate, decay
  deriving Show

-- | Loss functions
data LossFunction = LossFunction
  { computeLoss :: Vector Double -> Vector Double -> Double
  , lossDerivative :: Vector Double -> Vector Double -> Vector Double
  }

-- | Training configuration
data TrainingConfig = TrainingConfig
  { optimizer :: Optimizer
  , lossFunction :: LossFunction
  , epochs :: Int
  , batchSize :: Int
  }

-- | Activation function with its derivative
data ActivationFunction = ActivationFunction
  { activate :: Double -> Double
  , derivative :: Double -> Double
  }