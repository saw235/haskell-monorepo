module Main where

import Control.Monad (replicateM)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified NeuralNetwork as NN
import System.Random (Random, RandomGen, mkStdGen, randomR)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run)
import Text.Printf (printf)

-- =============================================================================
-- QUICKCHECK NEURAL NETWORK TESTS
-- =============================================================================

-- =============================================================================
-- HELPER FUNCTIONS FOR TESTING
-- =============================================================================

-- =============================================================================
-- CUSTOM GENERATORS
-- =============================================================================

-- Generate valid layer sizes (at least 2 layers, each with 1+ neurons)
genLayerSizes :: Gen [Int]
genLayerSizes = do
  numLayers <- choose (2, 5) -- 2-5 layers
  layerSizes <- replicateM numLayers (choose (1, 8)) -- 1-8 neurons per layer
  return layerSizes

-- Generate small layer sizes for detailed testing
genSmallLayerSizes :: Gen [Int]
genSmallLayerSizes = do
  numLayers <- choose (2, 3) -- 2-3 layers only
  layerSizes <- replicateM numLayers (choose (1, 4)) -- 1-4 neurons per layer
  return layerSizes

-- Generate a random vector of specified length
genVector :: Int -> Gen (Vector Double)
genVector n = do
  values <- replicateM n (choose (-2.0, 2.0))
  return (V.fromList values)

-- Generate a neural network with given layer sizes
genNeuralNetwork :: [Int] -> Gen NN.NeuralNetwork
genNeuralNetwork layerSizes = do
  seed <- arbitrary
  let gen = mkStdGen seed
      activations = replicate (length layerSizes - 1) NN.sigmoidActivation
      (network, _) = NN.createNetwork gen layerSizes activations
  return network

-- Generate a training example for given input size
genExample :: Int -> Gen NN.Example
genExample inputSize = do
  inputs <- genVector inputSize
  expectedOutputs <- genVector 1 -- Assume single output
  return $ NN.Example inputs expectedOutputs

-- =============================================================================
-- ACTIVATION FUNCTION PROPERTIES
-- =============================================================================

-- Property: Sigmoid output is always in range [0, 1]
prop_sigmoid_range :: Double -> Bool
prop_sigmoid_range x =
  let result = NN.activate NN.sigmoidActivation x
   in result >= 0 && result <= 1

-- Property: Sigmoid derivative is correctly computed
prop_sigmoid_derivative :: Double -> Bool
prop_sigmoid_derivative x =
  let sig = NN.activate NN.sigmoidActivation x
      expected = sig * (1 - sig)
      actual = NN.derivative NN.sigmoidActivation sig
      tolerance = 1e-10
   in abs (expected - actual) < tolerance

-- Property: ReLU is never negative
prop_relu_nonnegative :: Double -> Bool
prop_relu_nonnegative x =
  NN.activate NN.reluActivation x >= 0

-- Property: ReLU derivative is 0 or 1
prop_relu_derivative :: Double -> Bool
prop_relu_derivative x =
  let deriv = NN.derivative NN.reluActivation x
   in deriv == 0 || deriv == 1

-- Property: Tanh output is always in range [-1, 1]
prop_tanh_range :: Double -> Bool
prop_tanh_range x =
  let result = NN.activate NN.tanhActivation x
   in result >= -1 && result <= 1

-- =============================================================================
-- VECTOR AND DIMENSION PROPERTIES
-- =============================================================================

-- Property: Vector operations preserve dimensions
prop_vector_dimensions :: Int -> Property
prop_vector_dimensions n =
  n > 0 && n < 20 ==> monadicIO $ do
    vec <- run $ generate (genVector n)
    return $ V.length vec == n

-- Property: Parameter counting is correct
prop_parameter_count :: [Int] -> Property
prop_parameter_count layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    let NN.NeuralNetwork layers = network
        expectedParams =
          sum $
            zipWith
              (\inSize outSize -> outSize * inSize + outSize)
              layerSizes
              (tail layerSizes)
        actualParams = sum $ map countLayerParams layers
    return $ expectedParams == actualParams
  where
    countLayerParams (NN.Layer weights biases _) =
      V.length biases + V.sum (V.map V.length weights)

-- =============================================================================
-- FORWARD PROPAGATION PROPERTIES
-- =============================================================================

-- Property: Forward pass produces correct output dimensions
prop_forward_dimensions :: [Int] -> Property
prop_forward_dimensions layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    let output = NN.forward network input
        expectedOutputSize = last layerSizes
    return $ V.length output == expectedOutputSize

-- Property: Forward pass doesn't crash for valid inputs
prop_forward_no_crash :: [Int] -> Property
prop_forward_no_crash layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes && all (<= 10) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    let output = NN.forward network input
    -- Test passes if we get here without crashing
    return $ V.length output > 0

-- Property: Forward pass produces finite values
prop_forward_finite :: [Int] -> Property
prop_forward_finite layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes && all (<= 5) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    let output = NN.forward network input
    return $ V.all (\x -> not (isNaN x) && not (isInfinite x)) output

-- =============================================================================
-- BACKPROPAGATION PROPERTIES
-- =============================================================================

-- Property: Backward pass produces gradients with correct dimensions
prop_backward_dimensions :: [Int] -> Property
prop_backward_dimensions layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes && all (<= 5) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    target <- run $ generate (genVector (last layerSizes))
    let (weightGrads, biasGrads) = NN.backward network input target
        NN.NeuralNetwork layers = network
        weightDimsCorrect = and $ zipWith checkWeightDims layers weightGrads
        biasDimsCorrect = and $ zipWith checkBiasDims layers biasGrads
    return $ weightDimsCorrect && biasDimsCorrect
  where
    checkWeightDims (NN.Layer weights _ _) grads =
      V.length weights == V.length grads
        && V.and (V.zipWith (\w g -> V.length w == V.length g) weights grads)
    checkBiasDims (NN.Layer _ biases _) grads =
      V.length biases == V.length grads

-- Property: Backward pass doesn't crash
prop_backward_no_crash :: [Int] -> Property
prop_backward_no_crash layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes && all (<= 4) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    target <- run $ generate (genVector (last layerSizes))
    let (weightGrads, biasGrads) = NN.backward network input target
    -- Test passes if we get here without crashing
    return $
      length weightGrads == length layerSizes - 1
        && length biasGrads == length layerSizes - 1

-- Property: Activations computation produces correct number of layers
prop_activations_count :: [Int] -> Property
prop_activations_count layerSizes =
  length layerSizes >= 2 && all (> 0) layerSizes && all (<= 5) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    let activations = NN.computeActivations network input
        expectedCount = length layerSizes -- input + all layer outputs
    return $ length activations == expectedCount

-- =============================================================================
-- NUMERICAL GRADIENT TESTING
-- =============================================================================

-- Simple numerical gradient approximation
numericalGradient ::
  NN.NeuralNetwork ->
  Vector Double ->
  Vector Double ->
  Double ->
  ([Vector (Vector Double)], [Vector Double])
numericalGradient network inputs targets epsilon =
  let NN.NeuralNetwork layers = network
      loss net = NN.mse (NN.forward net inputs) targets

      -- Compute numerical weight gradients
      weightGrads = map (computeLayerWeightGrads loss network epsilon) [0 .. length layers - 1]

      -- Compute numerical bias gradients
      biasGrads = map (computeLayerBiasGrads loss network epsilon) [0 .. length layers - 1]
   in (weightGrads, biasGrads)
  where
    computeLayerWeightGrads lossFunc net eps layerIdx =
      let NN.NeuralNetwork layers = net
          layer = layers !! layerIdx
          NN.Layer weights biases activation = layer
       in V.imap
            ( \i neuronWeights ->
                V.imap
                  ( \j weight ->
                      let net1 = updateNetworkWeight net layerIdx i j (weight + eps)
                          net2 = updateNetworkWeight net layerIdx i j (weight - eps)
                          loss1 = lossFunc net1
                          loss2 = lossFunc net2
                       in (loss1 - loss2) / (2 * eps)
                  )
                  neuronWeights
            )
            weights

    computeLayerBiasGrads lossFunc net eps layerIdx =
      let NN.NeuralNetwork layers = net
          layer = layers !! layerIdx
          NN.Layer weights biases activation = layer
       in V.imap
            ( \i bias ->
                let net1 = updateNetworkBias net layerIdx i (bias + eps)
                    net2 = updateNetworkBias net layerIdx i (bias - eps)
                    loss1 = lossFunc net1
                    loss2 = lossFunc net2
                 in (loss1 - loss2) / (2 * eps)
            )
            biases

-- Helper functions to update network weights/biases for numerical gradients
updateNetworkWeight :: NN.NeuralNetwork -> Int -> Int -> Int -> Double -> NN.NeuralNetwork
updateNetworkWeight (NN.NeuralNetwork layers) layerIdx neuronIdx weightIdx newWeight =
  let updatedLayers =
        zipWith
          ( \i layer ->
              if i == layerIdx
                then updateLayerWeight layer neuronIdx weightIdx newWeight
                else layer
          )
          [0 ..]
          layers
   in NN.NeuralNetwork updatedLayers
  where
    updateLayerWeight (NN.Layer weights biases activation) nIdx wIdx newW =
      let updatedWeights =
            V.imap
              ( \i neuronWeights ->
                  if i == nIdx
                    then V.imap (\j w -> if j == wIdx then newW else w) neuronWeights
                    else neuronWeights
              )
              weights
       in NN.Layer updatedWeights biases activation

updateNetworkBias :: NN.NeuralNetwork -> Int -> Int -> Double -> NN.NeuralNetwork
updateNetworkBias (NN.NeuralNetwork layers) layerIdx neuronIdx newBias =
  let updatedLayers =
        zipWith
          ( \i layer ->
              if i == layerIdx
                then updateLayerBias layer neuronIdx newBias
                else layer
          )
          [0 ..]
          layers
   in NN.NeuralNetwork updatedLayers
  where
    updateLayerBias (NN.Layer weights biases activation) nIdx newB =
      let updatedBiases = V.imap (\i b -> if i == nIdx then newB else b) biases
       in NN.Layer weights updatedBiases activation

-- Property: Analytical gradients are close to numerical gradients
prop_gradient_correctness :: Property
prop_gradient_correctness = forAll genSmallLayerSizes $ \layerSizes ->
  length layerSizes >= 2 && all (> 0) layerSizes ==> monadicIO $ do
    network <- run $ generate (genNeuralNetwork layerSizes)
    input <- run $ generate (genVector (head layerSizes))
    target <- run $ generate (genVector (last layerSizes))
    let analytical = NN.backward network input target
        numerical = numericalGradient network input target 1e-4
        tolerance = 1e-2
    return $ gradientsClose analytical numerical tolerance
  where
    gradientsClose (wGrads1, bGrads1) (wGrads2, bGrads2) tol =
      weightsClose wGrads1 wGrads2 tol && biasesClose bGrads1 bGrads2 tol

    weightsClose wgs1 wgs2 tol = and $ zipWith (layerWeightsClose tol) wgs1 wgs2
    biasesClose bgs1 bgs2 tol = and $ zipWith (vectorClose tol) bgs1 bgs2

    layerWeightsClose tol wg1 wg2 = V.and $ V.zipWith (vectorClose tol) wg1 wg2
    vectorClose tol v1 v2 = V.and $ V.zipWith (\a b -> abs (a - b) < tol) v1 v2

-- =============================================================================
-- SPECIFIC BUG TESTS
-- =============================================================================

-- Property: Test the specific failing architecture [2,8,6,4,1]
prop_specific_architecture_no_crash :: Property
prop_specific_architecture_no_crash = monadicIO $ do
  let layerSizes = [2, 8, 6, 4, 1]
  network <- run $ generate (genNeuralNetwork layerSizes)
  input <- run $ generate (genVector 2)
  target <- run $ generate (genVector 1)

  -- Test NN.forward pass
  let output = NN.forward network input

  -- Test backward pass (this should not crash)
  let (weightGrads, biasGrads) = NN.backward network input target

  return $
    V.length output == 1
      && length weightGrads == 4
      && length biasGrads == 4

-- =============================================================================
-- MAIN FUNCTION
-- =============================================================================

main :: IO ()
main = do
  putStrLn "=== Neural Network Property Testing ==="
  putStrLn "Testing mathematical correctness and catching bugs"
  putStrLn ""

  putStrLn "1. Testing activation function properties..."
  quickCheck prop_sigmoid_range
  quickCheck prop_sigmoid_derivative
  quickCheck prop_relu_nonnegative
  quickCheck prop_relu_derivative
  quickCheck prop_tanh_range

  putStrLn "\n2. Testing vector and dimension properties..."
  quickCheck prop_vector_dimensions
  quickCheck prop_parameter_count

  putStrLn "\n3. Testing NN.forward propagation..."
  quickCheck prop_forward_dimensions
  quickCheck prop_forward_no_crash
  quickCheck prop_forward_finite

  putStrLn "\n4. Testing backpropagation..."
  quickCheck prop_backward_dimensions
  quickCheck prop_backward_no_crash
  quickCheck prop_activations_count

  putStrLn "\n5. Testing numerical gradient correctness..."
  putStrLn "This may take longer as it computes numerical gradients..."
  quickCheckWith stdArgs {maxSuccess = 10} prop_gradient_correctness

  putStrLn "\n6. Testing specific failing architecture [2,8,6,4,1]..."
  quickCheck prop_specific_architecture_no_crash

  putStrLn "\n=== Neural Network Tests Completed ==="
  putStrLn ""
  putStrLn "Key Properties Tested:"
  putStrLn "1. Activation function mathematical correctness"
  putStrLn "2. Dimension consistency across all operations"
  putStrLn "3. Forward propagation stability"
  putStrLn "4. Backpropagation correctness and crash safety"
  putStrLn "5. Gradient computation accuracy"
  putStrLn "6. Architecture-specific bug detection"
