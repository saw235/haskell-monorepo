module NeuralNetwork.Visualization where

import NeuralNetwork.Types
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)

-- | Neural network visualization monad
type NetworkViz = ReaderT NeuralNetwork IO

-- | Run a network visualization computation
runNetworkViz :: NetworkViz a -> NeuralNetwork -> IO a
runNetworkViz = runReaderT

-- | Print network structure and weights
printNetworkStructure :: NeuralNetwork -> IO ()
printNetworkStructure network = runNetworkViz printStructure network

printStructure :: NetworkViz ()
printStructure = do
  NeuralNetwork layers <- ask
  lift $ putStrLn "\n=== Neural Network Structure ==="
  lift $ putStrLn $ "Total layers: " ++ show (length layers)
  
  lift $ mapM_ (\(i, layer) -> do
    putStrLn $ "\n--- Layer " ++ show (i + 1 :: Int) ++ " ---"
    printLayerInfo i layer) (zip ([0..] :: [Int]) layers)

-- | Print detailed layer information
printLayerInfo :: Int -> Layer -> IO ()
printLayerInfo layerNum (Layer weights biases _) = do
  let numNeurons = V.length biases
      numInputs = if V.null weights then 0 else V.length (weights V.! 0)
  
  putStrLn $ "Neurons: " ++ show numNeurons ++ ", Inputs per neuron: " ++ show numInputs
  putStrLn "Biases:"
  V.iforM_ biases $ \i b -> 
    printf "  Neuron %d: %.4f\n" i b
  
  putStrLn "Weights (from input to neuron):"
  V.iforM_ weights $ \neuronIdx neuronWeights -> do
    printf "  Neuron %d weights: " neuronIdx
    V.mapM_ (\w -> printf "%.4f " w) neuronWeights
    putStrLn ""

-- | Print connectivity between layers
printConnectivity :: NeuralNetwork -> IO ()
printConnectivity network = runNetworkViz printConnectivityViz network

printConnectivityViz :: NetworkViz ()
printConnectivityViz = do
  NeuralNetwork layers <- ask
  lift $ putStrLn "\n=== Layer Connectivity ==="
  
  case layers of
    [] -> lift $ putStrLn "No layers in network"
    [layer] -> lift $ putStrLn "Single layer network (no hidden layers)"
    (Layer weights _ _ : _) -> do
      lift $ putStrLn "Network topology:"
      let inputSize = if V.null weights then 0 else V.length (weights V.! 0)
      
      lift $ putStr $ "Input(" ++ show inputSize ++ ")"
      
      lift $ mapM_ (\(i, Layer _ biases _) -> do
        let layerSize = V.length biases
        putStr $ " -> Layer" ++ show (i + 1 :: Int) ++ "(" ++ show layerSize ++ ")"
        ) (zip ([0..] :: [Int]) layers)
      
      lift $ putStrLn ""
      printWeightMatrixDimensions

printWeightMatrixDimensions :: NetworkViz ()
printWeightMatrixDimensions = do
  NeuralNetwork layers <- ask
  lift $ putStrLn "\nWeight matrix dimensions:"
  lift $ mapM_ (\(i, Layer weights biases _) -> do
    let numNeurons = V.length biases
        numInputs = if V.null weights then 0 else V.length (weights V.! 0)
    printf "Layer %d: [%d x %d] (neurons x inputs)\n" (i + 1 :: Int) numNeurons numInputs
    ) (zip ([0..] :: [Int]) layers)

-- | Print weight statistics for each layer
printWeightStats :: NeuralNetwork -> IO ()
printWeightStats network = runNetworkViz printWeightStatsViz network

printWeightStatsViz :: NetworkViz ()
printWeightStatsViz = do
  NeuralNetwork layers <- ask
  lift $ putStrLn "\n=== Weight Statistics ==="
  
  lift $ mapM_ (\(i, Layer weights biases _) -> do
    putStrLn $ "\nLayer " ++ show (i + 1 :: Int) ++ ":"
    
    -- Flatten all weights for statistics
    let allWeights = V.toList $ V.concat $ V.toList weights
        allBiases = V.toList biases
    
    case allWeights of
      [] -> return ()
      _ -> do
        let minWeight = minimum allWeights
            maxWeight = maximum allWeights
            avgWeight = sum allWeights / fromIntegral (length allWeights)
        
        printf "  Weights - Min: %.4f, Max: %.4f, Avg: %.4f\n" minWeight maxWeight avgWeight
    
    case allBiases of
      [] -> return ()
      _ -> do
        let minBias = minimum allBiases
            maxBias = maximum allBiases
            avgBias = sum allBiases / fromIntegral (length allBiases)
        
        printf "  Biases  - Min: %.4f, Max: %.4f, Avg: %.4f\n" minBias maxBias avgBias
    ) (zip ([0..] :: [Int]) layers)

-- | Combined network analysis using ReaderT
analyzeNetwork :: NetworkViz ()
analyzeNetwork = do
  printConnectivityViz
  printWeightStatsViz
  printStructure

-- | Convenient function to run full network analysis
runNetworkAnalysis :: NeuralNetwork -> IO ()
runNetworkAnalysis = runNetworkViz analyzeNetwork

-- | Quick network summary using ReaderT composition
quickSummary :: NetworkViz ()
quickSummary = do
  NeuralNetwork layers <- ask
  lift $ printf "Network: %d layers, %d total parameters\n" 
    (length layers) 
    (sum $ map countLayerParams layers)
  where
    countLayerParams (Layer weights biases _) = 
      V.length biases + V.sum (V.map V.length weights)

-- | Compare two networks using ReaderT
compareNetworkSizes :: NeuralNetwork -> NeuralNetwork -> IO ()
compareNetworkSizes net1 net2 = do
  putStrLn "\n=== Network Comparison ==="
  putStr "Network 1: "
  runNetworkViz quickSummary net1
  putStr "Network 2: "
  runNetworkViz quickSummary net2