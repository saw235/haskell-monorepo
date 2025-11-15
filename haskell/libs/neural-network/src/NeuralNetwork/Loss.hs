module NeuralNetwork.Loss where

import Data.Vector (Vector)
import qualified Data.Vector as V
import NeuralNetwork.Types

-- | Mean Squared Error loss function
mseLoss :: LossFunction
mseLoss =
  LossFunction
    { computeLoss = \outputs expected ->
        let squaredErrors = V.zipWith (\out exp -> (out - exp) ^ 2) outputs expected
         in V.sum squaredErrors / fromIntegral (V.length squaredErrors),
      lossDerivative = \outputs expected ->
        V.zipWith (\out exp -> 2 * (out - exp) / fromIntegral (V.length outputs)) outputs expected
    }

-- | Cross Entropy loss function
crossEntropyLoss :: LossFunction
crossEntropyLoss =
  LossFunction
    { computeLoss = \outputs expected ->
        let clipped = V.map (\x -> max 1e-15 (min (1 - 1e-15) x)) outputs
         in -V.sum (V.zipWith (\exp out -> exp * log out) expected clipped) / fromIntegral (V.length outputs),
      lossDerivative = \outputs expected ->
        let clipped = V.map (\x -> max 1e-15 (min (1 - 1e-15) x)) outputs
         in V.zipWith (\out exp -> -exp / out / fromIntegral (V.length outputs)) clipped expected
    }

-- | Compute mean squared error (legacy)
mse :: Vector Double -> Vector Double -> Double
mse = computeLoss mseLoss
