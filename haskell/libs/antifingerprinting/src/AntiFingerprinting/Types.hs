{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AntiFingerprinting.Types
  ( -- * Core Fingerprint Types
    Fingerprint (..),
    Navigator (..),
    Screen (..),
    UserAgent (..),
    WebGL (..),
    Canvas (..),

    -- * Constraint Types
    Device (..),
    OperatingSystem (..),
    Browser (..),
    BrowserConstraint (..),
    FingerprintOptions (..),

    -- * Header Types
    HttpHeader (..),
    HeaderName,
    HeaderValue,

    -- * Utility Types
    Locale,
    Version (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A complete browser fingerprint
data Fingerprint = Fingerprint
  { fingerprintUserAgent :: UserAgent,
    fingerprintNavigator :: Navigator,
    fingerprintScreen :: Screen,
    fingerprintWebGL :: WebGL,
    fingerprintCanvas :: Canvas,
    fingerprintHeaders :: Map HeaderName HeaderValue,
    fingerprintTimezone :: Text,
    fingerprintLocales :: [Locale]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Fingerprint

instance FromJSON Fingerprint

-- | Navigator properties (window.navigator)
data Navigator = Navigator
  { navigatorUserAgent :: Text,
    navigatorLanguage :: Text,
    navigatorLanguages :: [Text],
    navigatorPlatform :: Text,
    navigatorCookieEnabled :: Bool,
    navigatorDoNotTrack :: Maybe Text,
    navigatorHardwareConcurrency :: Int,
    navigatorMaxTouchPoints :: Int,
    navigatorVendor :: Text,
    navigatorVendorSub :: Text,
    navigatorProductSub :: Text,
    navigatorAppCodeName :: Text,
    navigatorAppName :: Text,
    navigatorAppVersion :: Text,
    navigatorBuildID :: Maybe Text,
    navigatorOscpu :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Navigator

instance FromJSON Navigator

-- | Screen properties (window.screen)
data Screen = Screen
  { screenWidth :: Int,
    screenHeight :: Int,
    screenAvailWidth :: Int,
    screenAvailHeight :: Int,
    screenColorDepth :: Int,
    screenPixelDepth :: Int,
    screenDevicePixelRatio :: Double,
    screenOrientationType :: Text,
    screenOrientationAngle :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Screen

instance FromJSON Screen

-- | User Agent string with parsed components
data UserAgent = UserAgent
  { userAgentString :: Text,
    userAgentBrowser :: Browser,
    userAgentVersion :: Version,
    userAgentOperatingSystem :: OperatingSystem,
    userAgentDevice :: Device
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserAgent

instance FromJSON UserAgent

-- | WebGL fingerprinting properties
data WebGL = WebGL
  { webglVendor :: Text,
    webglRenderer :: Text,
    webglVersion :: Text,
    webglShadingLanguageVersion :: Text,
    webglUnmaskedVendor :: Text,
    webglUnmaskedRenderer :: Text,
    webglSupportedExtensions :: [Text],
    webglParameters :: Map Text Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON WebGL

instance FromJSON WebGL

-- | Canvas fingerprinting properties
data Canvas = Canvas
  { canvasFingerprint :: Text,
    canvasNoiseEnabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Canvas

instance FromJSON Canvas

-- | Supported device types
data Device
  = Desktop
  | Mobile
  | Tablet
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON Device

instance FromJSON Device

-- | Supported operating systems
data OperatingSystem
  = Windows
  | MacOS
  | Linux
  | Android
  | IOS
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON OperatingSystem

instance FromJSON OperatingSystem

-- | Supported browsers
data Browser
  = Chrome
  | Firefox
  | Safari
  | Edge
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON Browser

instance FromJSON Browser

-- | Browser version constraints
data BrowserConstraint = BrowserConstraint
  { browserName :: Browser,
    browserMinVersion :: Maybe Version,
    browserMaxVersion :: Maybe Version
  }
  deriving (Show, Eq, Generic)

instance ToJSON BrowserConstraint

instance FromJSON BrowserConstraint

-- | Options for fingerprint generation
data FingerprintOptions = FingerprintOptions
  { optionsDevices :: [Device],
    optionsOperatingSystems :: [OperatingSystem],
    optionsBrowsers :: [BrowserConstraint],
    optionsLocales :: [Locale],
    optionsTimezones :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON FingerprintOptions

instance FromJSON FingerprintOptions

-- | HTTP header representation
data HttpHeader = HttpHeader
  { headerName :: HeaderName,
    headerValue :: HeaderValue
  }
  deriving (Show, Eq, Generic)

instance ToJSON HttpHeader

instance FromJSON HttpHeader

-- | Type aliases for clarity
type HeaderName = Text

type HeaderValue = Text

type Locale = Text

-- | Version representation
data Version = Version
  { versionMajor :: Int,
    versionMinor :: Int,
    versionPatch :: Int,
    versionBuild :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Version

instance FromJSON Version

-- | Compare versions
instance Ord Version where
  compare (Version maj1 min1 pat1 bld1) (Version maj2 min2 pat2 bld2) =
    case compare maj1 maj2 of
      EQ -> case compare min1 min2 of
        EQ -> case compare pat1 pat2 of
          EQ -> compare bld1 bld2
          result -> result
        result -> result
      result -> result
