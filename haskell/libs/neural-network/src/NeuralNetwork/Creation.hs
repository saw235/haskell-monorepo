module NeuralNetwork.Creation where

import NeuralNetwork.Types
import NeuralNetwork.Activation
import System.Random (Random, RandomGen, randomR)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Create a layer with random weights, biases, and activation function
createLayer :: RandomGen g => g -> Int -> Int -> ActivationFunction -> (Layer, g)
createLayer gen inputSize outputSize activationFn = (Layer weightsVec biasesVec activationFn, gen'')
  where
    -- Helper function to create a single weight row
    createWeightRow :: RandomGen g => g -> Int -> ([Double], g)
    createWeightRow g size = foldl (\(row, g) _ ->
      let (w, g') = randomR (-1.0, 1.0) g
      in (row ++ [w], g')) ([], g) [1..size]
    
    -- Helper function to create a single bias
    createBias :: RandomGen g => g -> (Double, g)
    createBias g = randomR (-1.0, 1.0) g
    
    -- Initialize weights with small random values (better range for sigmoid)
    (weightsList, gen') = foldl (\(weights, g) _ ->
      let (weightRow, g') = createWeightRow g inputSize
      in (weights ++ [V.fromList weightRow], g')) ([], gen) [1..outputSize]
    
    -- Initialize biases with small random values
    (biasesList, gen'') = foldl (\(biases, g) _ ->
      let (b, g') = createBias g
      in (biases ++ [b], g')) ([], gen') [1..outputSize]
    
    -- Convert lists to vectors
    weightsVec = V.fromList weightsList
    biasesVec = V.fromList biasesList

-- | Create a neural network with specified layer sizes and activations
createNetwork :: RandomGen g => g -> [Int] -> [ActivationFunction] -> (NeuralNetwork, g)
createNetwork gen layerSizes activations = (NeuralNetwork layers', gen')
  where
    (layers', gen') = foldl (\(layers, g) ((inputSize, outputSize), activationFn) ->
      let (layer, g') = createLayer g inputSize outputSize activationFn
      in (layers ++ [layer], g')) ([], gen) layerSpecs
    
    layerPairs = case layerSizes of
      [] -> []
      [_] -> []
      _ -> zip layerSizes (drop 1 layerSizes)
    layerSpecs = zip layerPairs activations

-- | Create a neural network with all sigmoid activations (legacy)
createNetworkSigmoid :: RandomGen g => g -> [Int] -> (NeuralNetwork, g)
createNetworkSigmoid gen layerSizes = 
  let activations = replicate (length layerSizes - 1) sigmoidActivation
  in createNetwork gen layerSizes activations

-- | Initialize a weight based on strategy
initWeight :: RandomGen g => g -> InitStrategy -> (Double, g)
initWeight gen strategy = case strategy of
  Uniform minVal maxVal -> randomR (minVal, maxVal) gen
  Normal mean stddev -> 
    let (r1, gen') = randomR (0, 1) gen
        (r2, gen'') = randomR (0, 1) gen'
        z = sqrt (-2 * log r1) * cos (2 * pi * r2)
    in (mean + stddev * z, gen'')
  Xavier fanIn -> 
    let bound = sqrt (1.0 / fromIntegral fanIn)
    in randomR (-bound, bound) gen
  He fanIn -> 
    let bound = sqrt (2.0 / fromIntegral fanIn)
    in randomR (-bound, bound) gen

-- | Create layer using LayerConfig
createLayerFromConfig :: RandomGen g => g -> Int -> LayerConfig -> (Layer, g)
createLayerFromConfig gen inputSize LayerConfig{..} = 
  createLayerWithInit gen inputSize layerSize layerActivation layerInitStrategy

-- | Create a layer with specified initialization
createLayerWithInit :: RandomGen g => g -> Int -> Int -> ActivationFunction -> InitStrategy -> (Layer, g)
createLayerWithInit gen inputSize outputSize activationFn initStrategy = 
  (Layer weightsVec biasesVec activationFn, gen'')
  where
    createWeightRow :: RandomGen g => g -> Int -> ([Double], g)
    createWeightRow g size = foldl (\(row, g) _ ->
      let (w, g') = initWeight g initStrategy
      in (row ++ [w], g')) ([], g) [1..size]
    
    createBias :: RandomGen g => g -> (Double, g)
    createBias g = initWeight g (Uniform (-0.1) 0.1)
    
    (weightsList, gen') = foldl (\(weights, g) _ ->
      let (weightRow, g') = createWeightRow g inputSize
      in (weights ++ [V.fromList weightRow], g')) ([], gen) [1..outputSize]
    
    (biasesList, gen'') = foldl (\(biases, g) _ ->
      let (b, g') = createBias g
      in (biases ++ [b], g')) ([], gen') [1..outputSize]
    
    weightsVec = V.fromList weightsList
    biasesVec = V.fromList biasesList

-- | Build network from layer configurations
buildNetwork :: RandomGen g => g -> Int -> [LayerConfig] -> (NeuralNetwork, g)
buildNetwork gen inputSize configs = (NeuralNetwork layers', gen')
  where
    sizes = inputSize : map layerSize configs
    layerSpecs = zip sizes configs
    
    (layers', gen') = foldl (\(layers, g) (inSize, config) ->
      let (layer, g') = createLayerFromConfig g inSize config
      in (layers ++ [layer], g')) ([], gen) layerSpecs