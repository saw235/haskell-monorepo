{-# LANGUAGE OverloadedStrings #-}

-- | FingerprintInjector - Inject browser fingerprints into WebDriver sessions
--
-- This library allows injecting realistic browser fingerprints into WebDriver
-- controlled browsers to make them appear more like real user browsers.
module FingerprintInjector
  ( -- * Injection Functions
    injectFingerprint,
    injectFingerprintWithOptions,
    patchWebDriver,

    -- * Individual Component Injection
    injectNavigator,
    injectScreen,
    injectWebGL,
    injectCanvas,

    -- * Verification Functions
    verifyFingerprint,
    queryNavigatorProperty,
    queryScreenProperty,
    queryWebGLProperty,

    -- * Injection Options
    InjectionOptions (..),
    defaultInjectionOptions,

    -- * Re-exports
    module AntiFingerprinting,
  )
where

import AntiFingerprinting
import Data.Aeson (ToJSON, Value, toJSON)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.WebDriver
import Test.WebDriver.Commands (executeJS)

-- | Options for fingerprint injection
data InjectionOptions = InjectionOptions
  { injectNavigatorProps :: Bool,
    injectScreenProps :: Bool,
    injectWebGLProps :: Bool,
    injectCanvasProps :: Bool,
    patchWebDriverFlag :: Bool,
    patchPlugins :: Bool
  }
  deriving (Show, Eq)

-- | Default injection options
defaultInjectionOptions :: InjectionOptions
defaultInjectionOptions =
  InjectionOptions
    { injectNavigatorProps = True,
      injectScreenProps = True,
      injectWebGLProps = True,
      injectCanvasProps = True,
      patchWebDriverFlag = True,
      patchPlugins = True
    }

-- | Inject a complete fingerprint into the WebDriver session
injectFingerprint :: Fingerprint -> WD ()
injectFingerprint = injectFingerprintWithOptions defaultInjectionOptions

-- | Inject a fingerprint with custom options
injectFingerprintWithOptions :: InjectionOptions -> Fingerprint -> WD ()
injectFingerprintWithOptions opts fingerprint = do
  -- Patch basic WebDriver detection
  when (patchWebDriverFlag opts) patchWebDriver

  -- Inject fingerprint components
  when (injectNavigatorProps opts) $ injectNavigator (fingerprintNavigator fingerprint)
  when (injectScreenProps opts) $ injectScreen (fingerprintScreen fingerprint)
  when (injectWebGLProps opts) $ injectWebGL (fingerprintWebGL fingerprint)
  when (injectCanvasProps opts) $ injectCanvas (fingerprintCanvas fingerprint)

  -- Additional patches
  when (patchPlugins opts) patchPluginDetection
  patchPermissions

  return ()

-- | Patch WebDriver detection (similar to nike-scraper)
patchWebDriver :: WD ()
patchWebDriver = do
  _ <- (executeJS [] jsWebDriverPatch :: WD ())
  return ()

-- | Inject navigator properties
injectNavigator :: Navigator -> WD ()
injectNavigator navigator = do
  let script = generateNavigatorScript navigator
  _ <- (executeJS [] script :: WD ())
  return ()

-- | Inject screen properties
injectScreen :: Screen -> WD ()
injectScreen screen = do
  let script = generateScreenScript screen
  _ <- (executeJS [] script :: WD ())
  return ()

-- | Inject WebGL properties
injectWebGL :: WebGL -> WD ()
injectWebGL webgl = do
  let script = generateWebGLScript webgl
  _ <- (executeJS [] script :: WD ())
  return ()

-- | Inject canvas fingerprint
injectCanvas :: Canvas -> WD ()
injectCanvas canvas = do
  let script = generateCanvasScript canvas
  _ <- (executeJS [] script :: WD ())
  return ()

-- | Patch plugin detection
patchPluginDetection :: WD ()
patchPluginDetection = do
  _ <- (executeJS [] jsPluginPatch :: WD ())
  return ()

-- | Patch permissions API
patchPermissions :: WD ()
patchPermissions = do
  _ <- (executeJS [] jsPermissionsPatch :: WD ())
  return ()

-- | Verify that fingerprint was injected correctly
verifyFingerprint :: Fingerprint -> WD Bool
verifyFingerprint fingerprint = do
  -- Check navigator properties
  navigatorOk <- verifyNavigator (fingerprintNavigator fingerprint)

  -- Check screen properties
  screenOk <- verifyScreen (fingerprintScreen fingerprint)

  -- Check WebGL properties
  webglOk <- verifyWebGL (fingerprintWebGL fingerprint)

  return (navigatorOk && screenOk && webglOk)

-- | Verify navigator properties
verifyNavigator :: Navigator -> WD Bool
verifyNavigator navigator = do
  actualUA <- queryNavigatorProperty "userAgent"
  actualPlatform <- queryNavigatorProperty "platform"
  actualVendor <- queryNavigatorProperty "vendor"

  let expectedUA = navigatorUserAgent navigator
      expectedPlatform = navigatorPlatform navigator
      expectedVendor = navigatorVendor navigator

  return $
    actualUA == expectedUA
      && actualPlatform == expectedPlatform
      && actualVendor == expectedVendor

-- | Verify screen properties
verifyScreen :: Screen -> WD Bool
verifyScreen screen = do
  actualWidth <- queryScreenProperty "width"
  actualHeight <- queryScreenProperty "height"

  let expectedWidth = T.pack $ show $ screenWidth screen
      expectedHeight = T.pack $ show $ screenHeight screen

  return $ actualWidth == expectedWidth && actualHeight == expectedHeight

-- | Verify WebGL properties
verifyWebGL :: WebGL -> WD Bool
verifyWebGL webgl = do
  actualVendor <- queryWebGLProperty "VENDOR"
  actualRenderer <- queryWebGLProperty "RENDERER"

  let expectedVendor = webglVendor webgl
      expectedRenderer = webglRenderer webgl

  return $ actualVendor == expectedVendor && actualRenderer == expectedRenderer

-- | Query a navigator property
queryNavigatorProperty :: Text -> WD Text
queryNavigatorProperty prop = do
  result <- executeJS [] $ "return navigator." <> prop <> ";"
  case result of
    Just (Aeson.String txt) -> return txt
    _ -> return ""

-- | Query a screen property
queryScreenProperty :: Text -> WD Text
queryScreenProperty prop = do
  result <- executeJS [] $ "return screen." <> prop <> ".toString();"
  case result of
    Just (Aeson.String txt) -> return txt
    _ -> return ""

-- | Query a WebGL property
queryWebGLProperty :: Text -> WD Text
queryWebGLProperty prop = do
  let script =
        T.unlines
          [ "try {",
            "  var canvas = document.createElement('canvas');",
            "  var gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');",
            "  if (gl) {",
            "    var debugInfo = gl.getExtension('WEBGL_debug_renderer_info');",
            "    if (debugInfo) {",
            "      return gl.getParameter(debugInfo." <> prop <> ");",
            "    }",
            "  }",
            "  return '';",
            "} catch (e) {",
            "  return '';",
            "}"
          ]
  result <- executeJS [] script
  case result of
    Just (Aeson.String txt) -> return txt
    _ -> return ""

-- | Generate JavaScript to patch WebDriver detection
jsWebDriverPatch :: Text
jsWebDriverPatch =
  T.unlines
    [ "try { Object.defineProperty(navigator, 'webdriver', {",
      "  get: () => false",
      "}); } catch (e) {}",
      "try { Object.defineProperty(navigator, 'plugins', {",
      "  get: () => [1, 2, 3, 4, 5]",
      "}); } catch (e) {}"
    ]

-- | Generate JavaScript to patch plugin detection
jsPluginPatch :: Text
jsPluginPatch =
  T.unlines
    [ "try { Object.defineProperty(navigator, 'plugins', {",
      "  get: () => [",
      "    {name: 'Chrome PDF Plugin', filename: 'internal-pdf-viewer'},",
      "    {name: 'Chrome PDF Viewer', filename: 'mhjfbmdgcfjbbpaeojofohoefgiehjai'},",
      "    {name: 'Native Client', filename: 'internal-nacl-plugin'}",
      "  ]",
      "}); } catch (e) {}"
    ]

-- | Generate JavaScript to patch permissions API
jsPermissionsPatch :: Text
jsPermissionsPatch =
  T.unlines
    [ "if (navigator.permissions && navigator.permissions.query) {",
      "  const originalQuery = navigator.permissions.query;",
      "  navigator.permissions.query = (parameters) => (",
      "    parameters.name === 'notifications' ?",
      "      Promise.resolve({ state: Notification.permission }) :",
      "      originalQuery(parameters)",
      "  );",
      "}"
    ]

-- | Generate JavaScript to inject navigator properties
generateNavigatorScript :: Navigator -> Text
generateNavigatorScript navigator =
  T.unlines $
    ["// Inject navigator properties"]
      ++ [ defineProperty "navigator" "userAgent" (quoted $ navigatorUserAgent navigator),
           defineProperty "navigator" "appVersion" (quoted $ navigatorAppVersion navigator),
           defineProperty "navigator" "appName" (quoted $ navigatorAppName navigator),
           defineProperty "navigator" "appCodeName" (quoted $ navigatorAppCodeName navigator),
           defineProperty "navigator" "product" "\"Gecko\"",
           defineProperty "navigator" "productSub" (quoted $ navigatorProductSub navigator),
           defineProperty "navigator" "vendor" (quoted $ navigatorVendor navigator),
           defineProperty "navigator" "vendorSub" (quoted $ navigatorVendorSub navigator),
           defineProperty "navigator" "platform" (quoted $ navigatorPlatform navigator),
           defineProperty "navigator" "language" (quoted $ navigatorLanguage navigator),
           defineProperty "navigator" "languages" (arrayValue $ navigatorLanguages navigator),
           defineProperty "navigator" "cookieEnabled" (boolValue $ navigatorCookieEnabled navigator),
           defineProperty "navigator" "hardwareConcurrency" (intValue $ navigatorHardwareConcurrency navigator),
           defineProperty "navigator" "maxTouchPoints" (intValue $ navigatorMaxTouchPoints navigator)
         ]
      ++ case navigatorDoNotTrack navigator of
        Just dnt -> [defineProperty "navigator" "doNotTrack" (quoted dnt)]
        Nothing -> []
      ++ case navigatorBuildID navigator of
        Just buildId -> [defineProperty "navigator" "buildID" (quoted buildId)]
        Nothing -> []
      ++ case navigatorOscpu navigator of
        Just oscpu -> [defineProperty "navigator" "oscpu" (quoted oscpu)]
        Nothing -> []

-- | Generate JavaScript to inject screen properties
generateScreenScript :: Screen -> Text
generateScreenScript screen =
  T.unlines
    [ "// Inject screen properties",
      defineProperty "screen" "width" (intValue $ screenWidth screen),
      defineProperty "screen" "height" (intValue $ screenHeight screen),
      defineProperty "screen" "availWidth" (intValue $ screenAvailWidth screen),
      defineProperty "screen" "availHeight" (intValue $ screenAvailHeight screen),
      defineProperty "screen" "colorDepth" (intValue $ screenColorDepth screen),
      defineProperty "screen" "pixelDepth" (intValue $ screenPixelDepth screen),
      defineProperty "screen" "orientation" orientationObj
    ]
  where
    orientationObj =
      T.concat
        [ "{ type: \"",
          screenOrientationType screen,
          "\"",
          ", angle: ",
          T.pack (show (screenOrientationAngle screen)),
          " }"
        ]

-- | Generate JavaScript to inject WebGL properties
generateWebGLScript :: WebGL -> Text
generateWebGLScript webgl =
  T.unlines
    [ "// Inject WebGL properties",
      "(() => {",
      "  const getParameter = WebGLRenderingContext.prototype.getParameter;",
      "  WebGLRenderingContext.prototype.getParameter = function(parameter) {",
      "    switch (parameter) {",
      "      case 37445: return '" <> webglVendor webgl <> "';", -- VENDOR
      "      case 37446: return '" <> webglRenderer webgl <> "';", -- RENDERER
      "      case 7936: return '" <> webglVersion webgl <> "';", -- VERSION
      "      case 35724: return '" <> webglShadingLanguageVersion webgl <> "';", -- SHADING_LANGUAGE_VERSION
      "    }",
      "    return getParameter.apply(this, arguments);",
      "  };",
      "  ",
      "  const getExtension = WebGLRenderingContext.prototype.getExtension;",
      "  WebGLRenderingContext.prototype.getExtension = function(name) {",
      "    if (name === 'WEBGL_debug_renderer_info') {",
      "      return {",
      "        UNMASKED_VENDOR_WEBGL: 37445,",
      "        UNMASKED_RENDERER_WEBGL: 37446",
      "      };",
      "    }",
      "    return getExtension.apply(this, arguments);",
      "  };",
      "})();"
    ]

-- | Generate JavaScript to inject canvas fingerprint
generateCanvasScript :: Canvas -> Text
generateCanvasScript canvas =
  T.unlines
    [ "// Inject canvas fingerprint",
      "const originalToDataURL = HTMLCanvasElement.prototype.toDataURL;",
      "const originalGetImageData = CanvasRenderingContext2D.prototype.getImageData;",
      "",
      "HTMLCanvasElement.prototype.toDataURL = function(...args) {",
      "  const result = originalToDataURL.apply(this, args);",
      "  // Add canvas noise if enabled",
      if canvasNoiseEnabled canvas
        then "  return result + '" <> canvasFingerprint canvas <> "';"
        else "  return result;",
      "};"
    ]

-- | Helper functions for JavaScript generation
defineProperty :: Text -> Text -> Text -> Text
defineProperty obj prop value =
  T.concat
    [ "Object.defineProperty(",
      obj,
      ", '",
      prop,
      "', { ",
      "get: () => (",
      value,
      "), ",
      "configurable: true, enumerable: true ",
      "});"
    ]

quoted :: Text -> Text
quoted txt = "\"" <> txt <> "\""

intValue :: Int -> Text
intValue = T.pack . show

boolValue :: Bool -> Text
boolValue True = "true"
boolValue False = "false"

arrayValue :: [Text] -> Text
arrayValue items = "[" <> T.intercalate ", " (map quoted items) <> "]"

-- | Conditional import helper
when :: Bool -> WD () -> WD ()
when True action = action
when False _ = return ()
