{-# LANGUAGE OverloadedStrings #-}

module AntiFingerprinting.Generator
  ( -- * Fingerprint Generation
    generateFingerprint
  , generateFingerprintWithOptions
  , generateFingerprintWithSeed
  
    -- * Component Generators
  , generateUserAgent
  , generateNavigator
  , generateScreen
  , generateWebGL
  , generateCanvas
  
    -- * Default Options
  , defaultFingerprintOptions
  ) where

import AntiFingerprinting.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, randomR, mkStdGen)
import Data.List (nub)

-- | Pick a random element from a non-empty list, returning the element and the new generator
pickRandom :: StdGen -> [a] -> (a, StdGen)
pickRandom gen xs = let (i, gen') = randomR (0, length xs - 1) gen in (xs !! i, gen')

-- | Generate a fingerprint with default options
generateFingerprint :: IO Fingerprint
generateFingerprint = generateFingerprintWithOptions defaultFingerprintOptions

-- | Generate a fingerprint with custom options
generateFingerprintWithOptions :: FingerprintOptions -> IO Fingerprint
generateFingerprintWithOptions options = do
  let seed = 42 -- Simplified for now
  return $ generateFingerprintWithSeed seed options

-- | Generate a fingerprint with a specific seed
generateFingerprintWithSeed :: Int -> FingerprintOptions -> Fingerprint
generateFingerprintWithSeed seed options = fingerprint
  where
    gen0 = mkStdGen seed
    -- Randomly select browser constraint
    (browserConstraint, gen1) = pickRandom gen0 (optionsBrowsers options)
    browser = browserName browserConstraint
    -- Randomly select OS
    (os, gen2) = pickRandom gen1 (optionsOperatingSystems options)
    -- Randomly select device
    (device, gen3) = pickRandom gen2 (optionsDevices options)
    -- Randomly select language
    (language, gen4) = pickRandom gen3 (optionsLocales options)
    -- Randomly select 2 unique locales
    (locale1, gen5) = pickRandom gen4 (optionsLocales options)
    (locale2, gen6) = pickRandom gen5 (filter (/= locale1) (optionsLocales options))
    locales = nub [locale1, locale2]
    -- Randomly select timezone
    (timezone, gen7) = pickRandom gen6 (optionsTimezones options)
    -- Generate version (could be randomized in future)
    version = Version 100 0 0 Nothing
    -- Generate user agent string
    userAgentStr = generateUserAgentString browser version os device
    userAgent = UserAgent
      { userAgentString = userAgentStr
      , userAgentBrowser = browser
      , userAgentVersion = version
      , userAgentOperatingSystem = os
      , userAgentDevice = device
      }
    navigator = generateNavigator userAgent language locales
    screen = generateScreen device
    webgl = generateWebGL browser os
    canvas = generateCanvas
    headers = Map.fromList
      [ ("User-Agent", userAgentString userAgent)
      , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      , ("Accept-Language", language)
      , ("Accept-Encoding", "gzip, deflate, br")
      ]
    fingerprint = Fingerprint
      { fingerprintUserAgent = userAgent
      , fingerprintNavigator = navigator
      , fingerprintScreen = screen
      , fingerprintWebGL = webgl
      , fingerprintCanvas = canvas
      , fingerprintHeaders = headers
      , fingerprintTimezone = timezone
      , fingerprintLocales = locales
      }

-- | Generate a user agent string and components
generateUserAgent :: StdGen -> FingerprintOptions -> UserAgent
generateUserAgent gen options = userAgent
  where
    -- Select browser based on constraints
    browserConstraint = head (optionsBrowsers options)
    browser = browserName browserConstraint
    
    -- Select operating system
    os = head (optionsOperatingSystems options)
    
    -- Select device
    device = head (optionsDevices options)
    
    -- Generate version
    version = Version 100 0 0 Nothing
    
    -- Generate user agent string
    userAgentStr = generateUserAgentString browser version os device
    
    userAgent = UserAgent
      { userAgentString = userAgentStr
      , userAgentBrowser = browser
      , userAgentVersion = version
      , userAgentOperatingSystem = os
      , userAgentDevice = device
      }

-- | Generate user agent string based on components
generateUserAgentString :: Browser -> Version -> OperatingSystem -> Device -> Text
generateUserAgentString Chrome _ Windows Desktop = 
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
generateUserAgentString Chrome _ MacOS Desktop =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
generateUserAgentString Firefox _ Windows Desktop =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/115.0"
generateUserAgentString Firefox _ MacOS Desktop =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/115.0"
generateUserAgentString Safari _ MacOS Desktop =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.1 Safari/605.1.15"
generateUserAgentString browser _ os device = 
  "Mozilla/5.0 (" <> formatOSDevice os device <> ") AppleWebKit/537.36 Chrome/120.0.0.0"

-- | Format OS and device for user agent string
formatOSDevice :: OperatingSystem -> Device -> Text
formatOSDevice Windows Desktop = "Windows NT 10.0; Win64; x64"
formatOSDevice MacOS Desktop = "Macintosh; Intel Mac OS X 10_15_7"
formatOSDevice Linux Desktop = "X11; Linux x86_64"
formatOSDevice Android Mobile = "Linux; Android 12; SM-G975F"
formatOSDevice IOS Mobile = "iPhone; CPU iPhone OS 15_0 like Mac OS X"
formatOSDevice os device = T.pack (show os) <> "; " <> T.pack (show device)

-- | Generate navigator properties
generateNavigator :: UserAgent -> Text -> [Text] -> Navigator
generateNavigator userAgent language languages = navigator
  where
    browser = userAgentBrowser userAgent
    os = userAgentOperatingSystem userAgent
    
    platform = generatePlatform os
    vendor = generateVendor browser
    
    navigator = Navigator
      { navigatorUserAgent = userAgentString userAgent
      , navigatorLanguage = language
      , navigatorLanguages = languages
      , navigatorPlatform = platform
      , navigatorCookieEnabled = True
      , navigatorDoNotTrack = Nothing
      , navigatorHardwareConcurrency = 8
      , navigatorMaxTouchPoints = 0
      , navigatorVendor = vendor
      , navigatorVendorSub = ""
      , navigatorProductSub = "20030107"
      , navigatorAppCodeName = "Mozilla"
      , navigatorAppName = "Netscape"
      , navigatorAppVersion = userAgentString userAgent
      , navigatorBuildID = generateBuildID browser
      , navigatorOscpu = generateOscpu os
      }

-- | Generate platform string
generatePlatform :: OperatingSystem -> Text
generatePlatform Windows = "Win32"
generatePlatform MacOS = "MacIntel"
generatePlatform Linux = "Linux x86_64"
generatePlatform Android = "Linux armv8l"
generatePlatform IOS = "iPhone"

-- | Generate vendor string
generateVendor :: Browser -> Text
generateVendor Chrome = "Google Inc."
generateVendor Edge = "Microsoft Corporation"
generateVendor Firefox = ""
generateVendor Safari = "Apple Computer, Inc."

-- | Generate build ID for Firefox
generateBuildID :: Browser -> Maybe Text
generateBuildID Firefox = Just "20181001000000"
generateBuildID _ = Nothing

-- | Generate OSCPU for Firefox
generateOscpu :: OperatingSystem -> Maybe Text
generateOscpu Windows = Just "Windows NT 10.0; Win64; x64"
generateOscpu MacOS = Just "Intel Mac OS X 10.15"
generateOscpu Linux = Just "Linux x86_64"
generateOscpu _ = Nothing

-- | Generate screen properties
generateScreen :: Device -> Screen
generateScreen device = screen
  where
    (width, height, ratio) = case device of
      Desktop -> (1920, 1080, 1.0)
      Mobile -> (375, 667, 2.0)
      Tablet -> (768, 1024, 1.5)
    
    screen = Screen
      { screenWidth = width
      , screenHeight = height
      , screenAvailWidth = width
      , screenAvailHeight = height - 40
      , screenColorDepth = 24
      , screenPixelDepth = 24
      , screenDevicePixelRatio = ratio
      , screenOrientationType = if width > height then "landscape-primary" else "portrait-primary"
      , screenOrientationAngle = if width > height then 0 else 90
      }

-- | Generate WebGL properties
generateWebGL :: Browser -> OperatingSystem -> WebGL
generateWebGL browser os = webgl
  where
    vendor = generateWebGLVendor os
    renderer = generateWebGLRenderer os
    
    webgl = WebGL
      { webglVendor = vendor
      , webglRenderer = renderer
      , webglVersion = "WebGL 1.0"
      , webglShadingLanguageVersion = "WebGL GLSL ES 1.0"
      , webglUnmaskedVendor = vendor
      , webglUnmaskedRenderer = renderer
      , webglSupportedExtensions = generateWebGLExtensions browser
      , webglParameters = Map.empty
      }

-- | Generate WebGL vendor
generateWebGLVendor :: OperatingSystem -> Text
generateWebGLVendor Windows = "Microsoft Corporation"
generateWebGLVendor MacOS = "Apple Inc."
generateWebGLVendor _ = "Mesa/X.org"

-- | Generate WebGL renderer
generateWebGLRenderer :: OperatingSystem -> Text
generateWebGLRenderer Windows = "ANGLE (NVIDIA GeForce GTX 1060 Direct3D11 vs_5_0 ps_5_0)"
generateWebGLRenderer MacOS = "Apple M1"
generateWebGLRenderer _ = "Mesa DRI Intel(R) UHD Graphics 620 (KBL GT2)"

-- | Generate WebGL extensions
generateWebGLExtensions :: Browser -> [Text]
generateWebGLExtensions Chrome = 
  [ "ANGLE_instanced_arrays"
  , "EXT_blend_minmax"
  , "OES_element_index_uint"
  , "OES_standard_derivatives"
  , "OES_texture_float"
  , "WEBGL_compressed_texture_s3tc"
  , "WEBGL_debug_renderer_info"
  , "WEBGL_depth_texture"
  , "WEBGL_lose_context"
  ]
generateWebGLExtensions Firefox = 
  [ "EXT_blend_minmax"
  , "OES_element_index_uint"
  , "OES_standard_derivatives"
  , "OES_texture_float"
  , "WEBGL_compressed_texture_s3tc"
  , "WEBGL_debug_renderer_info"
  , "WEBGL_depth_texture"
  , "WEBGL_lose_context"
  ]
generateWebGLExtensions _ = generateWebGLExtensions Chrome

-- | Generate canvas fingerprint
generateCanvas :: Canvas
generateCanvas = Canvas
  { canvasFingerprint = "1234567890"
  , canvasNoiseEnabled = False
  }

-- | Default fingerprint options
defaultFingerprintOptions :: FingerprintOptions
defaultFingerprintOptions = FingerprintOptions
  { optionsDevices = [Desktop]
  , optionsOperatingSystems = [Windows, MacOS, Linux]
  , optionsBrowsers = 
      [ BrowserConstraint Chrome (Just (Version 90 0 0 Nothing)) Nothing
      , BrowserConstraint Firefox (Just (Version 88 0 0 Nothing)) Nothing
      , BrowserConstraint Safari (Just (Version 14 0 0 Nothing)) Nothing
      ]
  , optionsLocales = ["en-US", "en"]
  , optionsTimezones = ["America/New_York", "Europe/London", "Asia/Tokyo"]
  } 