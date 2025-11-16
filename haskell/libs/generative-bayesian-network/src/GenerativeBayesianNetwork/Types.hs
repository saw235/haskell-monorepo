{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GenerativeBayesianNetwork.Types
  ( -- * Core Network Types
    BayesianNetwork (..),
    Node (..),
    NodeType (..),
    Distribution (..),
    Probability,
    Evidence (..),

    -- * Fingerprint-specific Types
    BrowserFeature (..),
    FeatureValue (..),
    FeatureDependency (..),
    ConditionalProbability (..),

    -- * Network Structure
    NetworkStructure (..),
    NodeConnections (..),
    EdgeWeight,

    -- * Generation Options
    GenerationOptions (..),
    SamplingMethod (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Type alias for probability values (0.0 to 1.0)
type Probability = Double

-- | Type alias for edge weights in the network
type EdgeWeight = Double

-- | A complete Bayesian network for fingerprint generation
data BayesianNetwork = BayesianNetwork
  { networkNodes :: Map Text Node,
    networkStructure :: NetworkStructure,
    networkEvidence :: [Evidence]
  }
  deriving (Show, Eq, Generic)

instance ToJSON BayesianNetwork

instance FromJSON BayesianNetwork

-- | A node in the Bayesian network representing a browser feature
data Node = Node
  { nodeId :: Text,
    nodeType :: NodeType,
    nodeFeature :: BrowserFeature,
    nodeDistribution :: Distribution,
    nodeParents :: [Text],
    nodeChildren :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Node

instance FromJSON Node

-- | Type of node in the network
data NodeType
  = RootNode -- No parents
  | ConditionalNode -- Has parents
  | LeafNode -- No children
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON NodeType

instance FromJSON NodeType

-- | Probability distribution for a node
data Distribution
  = DiscreteDistribution [(FeatureValue, Probability)]
  | ConditionalDistribution [ConditionalProbability]
  | ContinuousDistribution
      { distributionMean :: Double,
        distributionStdDev :: Double,
        distributionMin :: Maybe Double,
        distributionMax :: Maybe Double
      }
  deriving (Show, Eq, Generic)

instance ToJSON Distribution

instance FromJSON Distribution

-- | Evidence (observed values) for network inference
data Evidence = Evidence
  { evidenceFeature :: BrowserFeature,
    evidenceValue :: FeatureValue,
    evidenceStrength :: Probability
  }
  deriving (Show, Eq, Generic)

instance ToJSON Evidence

instance FromJSON Evidence

-- | Browser features that can be modeled in the network
data BrowserFeature
  = UserAgentFeature
  | ScreenResolutionFeature
  | TimezoneFeature
  | LanguageFeature
  | PlatformFeature
  | WebGLVendorFeature
  | WebGLRendererFeature
  | CanvasFingerprintFeature
  | HardwareConcurrencyFeature
  | DevicePixelRatioFeature
  | TouchSupportFeature
  | ColorDepthFeature
  | PluginSupportFeature
  | CookieEnabledFeature
  | DoNotTrackFeature
  | Custom Text -- For extensibility
  deriving (Show, Eq, Ord, Generic)

instance ToJSON BrowserFeature

instance FromJSON BrowserFeature

-- | Possible values for browser features
data FeatureValue
  = TextValue Text
  | NumericValue Double
  | BooleanValue Bool
  | ListValue [FeatureValue]
  | ResolutionValue (Int, Int)
  deriving (Show, Eq, Generic)

instance ToJSON FeatureValue

instance FromJSON FeatureValue

-- | Dependency between features
data FeatureDependency = FeatureDependency
  { dependencyParent :: BrowserFeature,
    dependencyChild :: BrowserFeature,
    dependencyStrength :: EdgeWeight
  }
  deriving (Show, Eq, Generic)

instance ToJSON FeatureDependency

instance FromJSON FeatureDependency

-- | Conditional probability table entry
data ConditionalProbability = ConditionalProbability
  { conditionalParentValues :: [(BrowserFeature, FeatureValue)],
    conditionalChildValue :: FeatureValue,
    conditionalProbability :: Probability
  }
  deriving (Show, Eq, Generic)

instance ToJSON ConditionalProbability

instance FromJSON ConditionalProbability

-- | Network structure definition
data NetworkStructure = NetworkStructure
  { structureDependencies :: [FeatureDependency],
    structureConnections :: Map Text NodeConnections
  }
  deriving (Show, Eq, Generic)

instance ToJSON NetworkStructure

instance FromJSON NetworkStructure

-- | Connections for a specific node
data NodeConnections = NodeConnections
  { connectionsIncoming :: [(Text, EdgeWeight)],
    connectionsOutgoing :: [(Text, EdgeWeight)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON NodeConnections

instance FromJSON NodeConnections

-- | Options for generating samples from the network
data GenerationOptions = GenerationOptions
  { optionsSamplingMethod :: SamplingMethod,
    optionsNumSamples :: Int,
    optionsEvidence :: [Evidence],
    optionsRandomSeed :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON GenerationOptions

instance FromJSON GenerationOptions

-- | Methods for sampling from the network
data SamplingMethod
  = ForwardSampling -- Simple forward sampling
  | GibbsSampling -- Gibbs sampling for conditional inference
  | MetropolisHastings -- Metropolis-Hastings algorithm
  | WeightedSampling -- Likelihood weighting
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON SamplingMethod

instance FromJSON SamplingMethod
