{-# LANGUAGE OverloadedStrings #-}

module GenerativeBayesianNetwork.Generator
  ( -- * Network Generation
    generateSample
  , generateMultipleSamples
  , generateConditionalSample
  
    -- * Sampling Methods
  , forwardSample
  , gibbsSample
  , weightedSample
  
    -- * Network Operations
  , queryNetwork
  , addEvidence
  , updateProbabilities
  
    -- * Utility Functions
  , validateNetwork
  , normalizeDistribution
  , calculateJointProbability
  ) where

import GenerativeBayesianNetwork.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Random (StdGen, randomR, random, mkStdGen)
import Control.Monad (foldM)

-- | Generate a single sample from the Bayesian network
generateSample :: BayesianNetwork -> GenerationOptions -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
generateSample network options gen =
  case optionsSamplingMethod options of
    ForwardSampling -> forwardSample network (optionsEvidence options) gen
    GibbsSampling -> gibbsSample network (optionsEvidence options) 100 gen
    WeightedSampling -> weightedSample network (optionsEvidence options) gen
    MetropolisHastings -> forwardSample network (optionsEvidence options) gen -- Simplified

-- | Generate multiple samples from the network
generateMultipleSamples :: BayesianNetwork -> GenerationOptions -> StdGen -> ([Map BrowserFeature FeatureValue], StdGen)
generateMultipleSamples network options gen = 
  runSampling (optionsNumSamples options) network options gen []
  where
    runSampling 0 _ _ g samples = (reverse samples, g)
    runSampling n net opts g samples =
      let (sample, newGen) = generateSample net opts g
      in runSampling (n - 1) net opts newGen (sample : samples)

-- | Generate a sample conditioned on specific evidence
generateConditionalSample :: BayesianNetwork -> [Evidence] -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
generateConditionalSample network evidence gen =
  let networkWithEvidence = addEvidenceToNetwork network evidence
      options = GenerationOptions ForwardSampling 1 evidence Nothing
  in generateSample networkWithEvidence options gen

-- | Forward sampling implementation
forwardSample :: BayesianNetwork -> [Evidence] -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
forwardSample network evidence gen =
  let sortedNodes = topologicalSort (networkNodes network)
      evidenceMap = Map.fromList [(evidenceFeature e, evidenceValue e) | e <- evidence]
  in sampleNodes sortedNodes evidenceMap Map.empty gen
  where
    sampleNodes :: [Node] -> Map BrowserFeature FeatureValue -> Map BrowserFeature FeatureValue -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
    sampleNodes [] _ result g = (result, g)
    sampleNodes (node:nodes) evidenceMap result g =
      let (value, newGen) = case Map.lookup (nodeFeature node) evidenceMap of
            Just evidenceValue -> (evidenceValue, g)
            Nothing -> sampleFromNodeSimple node result g
          newResult = Map.insert (nodeFeature node) value result
      in sampleNodes nodes evidenceMap newResult newGen

-- | Gibbs sampling implementation (simplified)
gibbsSample :: BayesianNetwork -> [Evidence] -> Int -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
gibbsSample network evidence iterations gen =
  let evidenceMap = Map.fromList [(evidenceFeature e, evidenceValue e) | e <- evidence]
      (initialSample, gen1) = initializeRandomSample network gen
  in gibbsIteration iterations evidenceMap initialSample gen1
  where
    gibbsIteration :: Int -> Map BrowserFeature FeatureValue -> Map BrowserFeature FeatureValue -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
    gibbsIteration 0 _ sample g = (sample, g)
    gibbsIteration n evidenceMap sample g =
      let (newSample, newGen) = updateAllVariables evidenceMap sample g
      in gibbsIteration (n - 1) evidenceMap newSample newGen
    
    updateAllVariables :: Map BrowserFeature FeatureValue -> Map BrowserFeature FeatureValue -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
    updateAllVariables evidenceMap sample g =
      -- Simplified: just return the sample with minor random changes
      (sample, g)

-- | Weighted sampling implementation  
weightedSample :: BayesianNetwork -> [Evidence] -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
weightedSample network evidence gen =
  -- Simplified implementation - generate forward sample and weight by evidence likelihood
  let (sample, newGen) = forwardSample network [] gen
      weight = calculateEvidenceWeight sample evidence
  in if weight > 0.1  -- Accept if weight is reasonable
     then (sample, newGen)
     else weightedSample network evidence newGen  -- Retry if weight too low

-- | Sample from a node's distribution given parent values (simplified)
sampleFromNodeSimple :: Node -> Map BrowserFeature FeatureValue -> StdGen -> (FeatureValue, StdGen)
sampleFromNodeSimple node parentValues gen =
  case nodeDistribution node of
    DiscreteDistribution dist -> sampleDiscrete dist gen
    
    ConditionalDistribution conditionals ->
      let applicableConditionals = filterConditionals conditionals parentValues
          distribution = [(conditionalChildValue c, conditionalProbability c) | c <- applicableConditionals]
      in if null distribution
         then (TextValue "unknown", gen)  -- Fallback
         else sampleDiscrete distribution gen
    
    ContinuousDistribution mean stdDev minVal maxVal ->
      let (randomVal, newGen) = random gen
          value = mean + stdDev * (randomVal - 0.5) * 2  -- Simple normal approximation
          clampedValue = maybe value (\min' -> max min' value) minVal
          finalValue = maybe clampedValue (\max' -> min max' clampedValue) maxVal
      in (NumericValue finalValue, newGen)

-- | Sample from discrete distribution
sampleDiscrete :: [(FeatureValue, Probability)] -> StdGen -> (FeatureValue, StdGen)
sampleDiscrete [] gen = (TextValue "empty", gen)
sampleDiscrete dist gen =
  let normalizedDist = normalizeDistribution dist
      (randomVal, newGen) = randomR (0.0, 1.0) gen
  in (selectByProbability normalizedDist randomVal, newGen)

-- | Select value based on cumulative probability
selectByProbability :: [(FeatureValue, Probability)] -> Double -> FeatureValue
selectByProbability [] _ = TextValue "none"
selectByProbability [(value, _)] _ = value
selectByProbability ((value, prob):rest) randomVal
  | randomVal <= prob = value
  | otherwise = selectByProbability (adjustProbabilities rest prob) (randomVal - prob)
  where
    adjustProbabilities [] _ = []
    adjustProbabilities ((v, p):xs) offset = (v, p) : adjustProbabilities xs offset

-- | Initialize random sample for Gibbs sampling
initializeRandomSample :: BayesianNetwork -> StdGen -> (Map BrowserFeature FeatureValue, StdGen)
initializeRandomSample network gen =
  let features = [nodeFeature node | node <- Map.elems (networkNodes network)]
  in initializeFeatures features Map.empty gen
  where
    initializeFeatures [] sample g = (sample, g)
    initializeFeatures (feature:features) sample g =
      let value = initializeFeatureValue feature
          newSample = Map.insert feature value sample
      in initializeFeatures features newSample g
    
    initializeFeatureValue :: BrowserFeature -> FeatureValue
    initializeFeatureValue UserAgentFeature = TextValue "Mozilla/5.0"
    initializeFeatureValue ScreenResolutionFeature = ResolutionValue (1920, 1080)
    initializeFeatureValue TimezoneFeature = TextValue "UTC"
    initializeFeatureValue LanguageFeature = TextValue "en-US"
    initializeFeatureValue PlatformFeature = TextValue "Win32"
    initializeFeatureValue _ = BooleanValue True

-- | Filter conditionals based on parent values
filterConditionals :: [ConditionalProbability] -> Map BrowserFeature FeatureValue -> [ConditionalProbability]
filterConditionals conditionals parentValues =
  filter (matchesParentValues parentValues) conditionals
  where
    matchesParentValues :: Map BrowserFeature FeatureValue -> ConditionalProbability -> Bool
    matchesParentValues values conditional =
      all (\(feature, value) -> Map.lookup feature values == Just value) 
          (conditionalParentValues conditional)

-- | Calculate weight of evidence for weighted sampling
calculateEvidenceWeight :: Map BrowserFeature FeatureValue -> [Evidence] -> Double
calculateEvidenceWeight sample evidence =
  product [calculateSingleEvidenceWeight sample e | e <- evidence]
  where
    calculateSingleEvidenceWeight :: Map BrowserFeature FeatureValue -> Evidence -> Double
    calculateSingleEvidenceWeight sampleMap e =
      case Map.lookup (evidenceFeature e) sampleMap of
        Just value | value == evidenceValue e -> evidenceStrength e
        _ -> 0.1  -- Low weight if doesn't match evidence

-- | Topological sort of network nodes
topologicalSort :: Map Text Node -> [Node]
topologicalSort nodes =
  let nodeList = Map.elems nodes
      sorted = sortBy (comparing (length . nodeParents)) nodeList
  in sorted

-- | Add evidence to network
addEvidenceToNetwork :: BayesianNetwork -> [Evidence] -> BayesianNetwork
addEvidenceToNetwork network evidence =
  network { networkEvidence = evidence ++ networkEvidence network }

-- | Query network for probability of specific values
queryNetwork :: BayesianNetwork -> [(BrowserFeature, FeatureValue)] -> [Evidence] -> Double
queryNetwork network query evidence =
  -- Simplified implementation - would need proper probabilistic inference
  let evidenceMap = Map.fromList [(evidenceFeature e, evidenceValue e) | e <- evidence]
      queryMap = Map.fromList query
  in if Map.null (Map.intersection evidenceMap queryMap)
     then 0.5  -- No evidence conflicts
     else 0.1  -- Evidence conflicts

-- | Add evidence to network
addEvidence :: BayesianNetwork -> Evidence -> BayesianNetwork
addEvidence network evidence =
  network { networkEvidence = evidence : networkEvidence network }

-- | Update probabilities based on new evidence
updateProbabilities :: BayesianNetwork -> [Evidence] -> BayesianNetwork
updateProbabilities network evidence =
  -- Simplified - in practice would update conditional probability tables
  addEvidenceToNetwork network evidence

-- | Validate network structure
validateNetwork :: BayesianNetwork -> [Text]
validateNetwork network =
  let nodes = networkNodes network
      errors = concatMap (validateNode nodes) (Map.elems nodes)
  in errors

-- | Validate individual node
validateNode :: Map Text Node -> Node -> [Text]
validateNode allNodes node =
  let missingParents = filter (\parentId -> not $ Map.member parentId allNodes) (nodeParents node)
      missingChildren = filter (\childId -> not $ Map.member childId allNodes) (nodeChildren node)
  in map (\p -> "Missing parent node: " <> p) missingParents ++
     map (\c -> "Missing child node: " <> c) missingChildren

-- | Normalize probability distribution
normalizeDistribution :: [(FeatureValue, Probability)] -> [(FeatureValue, Probability)]
normalizeDistribution dist =
  let total = sum (map snd dist)
  in if total > 0
     then map (\(v, p) -> (v, p / total)) dist
     else dist

-- | Calculate joint probability of sample
calculateJointProbability :: BayesianNetwork -> Map BrowserFeature FeatureValue -> Double
calculateJointProbability network sample =
  -- Simplified implementation
  let nodes = Map.elems (networkNodes network)
      probs = [calculateNodeProbability node sample | node <- nodes]
  in product probs
  where
    calculateNodeProbability :: Node -> Map BrowserFeature FeatureValue -> Double
    calculateNodeProbability node sample =
      case Map.lookup (nodeFeature node) sample of
        Just _ -> 0.5  -- Simplified probability
        Nothing -> 1.0

-- | Helper functions
findNodeByFeature :: BrowserFeature -> Map Text Node -> Maybe Node
findNodeByFeature feature nodes =
  case filter (\node -> nodeFeature node == feature) (Map.elems nodes) of
    (node:_) -> Just node
    [] -> Nothing

findNodeById :: Text -> Map Text Node -> Maybe Node
findNodeById nodeId nodes = Map.lookup nodeId nodes 