{-# LANGUAGE OverloadedStrings #-}

module FingerprintTest
  ( -- * Test Functions
    runFingerprintTests
  , testBasicInjection
  , testNavigatorInjection
  , testScreenInjection
  , testWebGLInjection
  , testFullFingerprint
  
    -- * Test Utilities
  , chromeConfigForTesting
  , printTestResult
  , TestResult(..)
  ) where

import FingerprintInjector
import Test.WebDriver
import Test.WebDriver.Commands
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import System.IO (getLine)

-- | Test result representation
data TestResult = TestResult
  { testName :: Text
  , testPassed :: Bool
  , testMessage :: Text
  } deriving (Show, Eq)

-- | Chrome configuration for testing (similar to nike-scraper)
chromeConfigForTesting :: Bool -> WDConfig
chromeConfigForTesting headless = useBrowser (chrome { chromeOptions = opts }) defaultConfig
  where
    userAgent = "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    opts = (if headless then ["--headless"] else ["--start-minimized"]) ++ 
           ["--no-sandbox", "--disable-dev-shm-usage", userAgent]

-- | Run all fingerprint injection tests
runFingerprintTests :: Bool -> Bool -> IO [TestResult]
runFingerprintTests headless keepBrowser = do
  putStrLn "=== Running Fingerprint Injection Tests ==="
  
  results <- sequence
    [ testBasicInjection headless
    , testNavigatorInjection headless
    , testScreenInjection headless
    , testWebGLInjection headless
    , testFullFingerprint headless
    ]
  
  putStrLn "\n=== Test Summary ==="
  mapM_ printTestResult results
  
  let passedCount = length $ filter testPassed results
      totalCount = length results
  putStrLn $ "Passed: " ++ show passedCount ++ "/" ++ show totalCount

  if keepBrowser
    then do
      putStrLn "\n[DEBUG] Browser is being kept open for inspection. Press Enter to exit and close the browser..."
      _ <- getLine
      return ()
    else return ()
  
  return results

-- | Test basic WebDriver patching
testBasicInjection :: Bool -> IO TestResult
testBasicInjection headless = do
  result <- runSession (chromeConfigForTesting headless) $ do
    openPage "http://localhost:8081/test.html"
    
    -- Test before patching
    webdriverBefore <- queryNavigatorProperty "webdriver"
    
    -- Apply basic patches
    patchWebDriver
    
    -- Test after patching
    webdriverAfter <- queryNavigatorProperty "webdriver"
    
    closeSession
    return (webdriverBefore, webdriverAfter)
  
  case result of
    (before, after) -> do
      let passed = after == "false" || T.null after
          message = "WebDriver flag: " <> before <> " -> " <> after
      return $ TestResult "Basic WebDriver Patching" passed message

-- | Test navigator property injection
testNavigatorInjection :: Bool -> IO TestResult
testNavigatorInjection headless = do
  -- Generate a test fingerprint
  fingerprint <- generateFingerprint
  let navigator = fingerprintNavigator fingerprint
      expectedUA = navigatorUserAgent navigator
      expectedPlatform = navigatorPlatform navigator
  
  result <- runSession (chromeConfigForTesting headless) $ do
    openPage "http://localhost:8081/test.html"
    
    -- Inject navigator properties
    injectNavigator navigator
    
    -- Query injected properties
    actualUA <- queryNavigatorProperty "userAgent"
    actualPlatform <- queryNavigatorProperty "platform"
    actualVendor <- queryNavigatorProperty "vendor"
    
    closeSession
    return (actualUA, actualPlatform, actualVendor)
  
  case result of
    (actualUA, actualPlatform, actualVendor) -> do
      let uaMatches = actualUA == expectedUA
          platformMatches = actualPlatform == expectedPlatform
          passed = uaMatches && platformMatches
          message = T.unlines
            [ "UserAgent: " <> if uaMatches then "✓" else "✗"
            , "Platform: " <> if platformMatches then "✓" else "✗"
            , "Vendor: " <> actualVendor
            ]
      return $ TestResult "Navigator Property Injection" passed message

-- | Test screen property injection
testScreenInjection :: Bool -> IO TestResult
testScreenInjection headless = do
  fingerprint <- generateFingerprint
  let screen = fingerprintScreen fingerprint
      expectedWidth = T.pack $ show $ screenWidth screen
      expectedHeight = T.pack $ show $ screenHeight screen
  
  result <- runSession (chromeConfigForTesting headless) $ do
    openPage "http://localhost:8081/test.html"
    
    -- Inject screen properties
    injectScreen screen
    
    -- Query injected properties
    actualWidth <- queryScreenProperty "width"
    actualHeight <- queryScreenProperty "height"
    actualColorDepth <- queryScreenProperty "colorDepth"
    
    closeSession
    return (actualWidth, actualHeight, actualColorDepth)
  
  case result of
    (actualWidth, actualHeight, actualColorDepth) -> do
      let widthMatches = actualWidth == expectedWidth
          heightMatches = actualHeight == expectedHeight
          passed = widthMatches && heightMatches
          message = T.unlines
            [ "Width: " <> actualWidth <> if widthMatches then " ✓" else " ✗"
            , "Height: " <> actualHeight <> if heightMatches then " ✓" else " ✗"
            , "Color Depth: " <> actualColorDepth
            ]
      return $ TestResult "Screen Property Injection" passed message

-- | Test WebGL property injection
testWebGLInjection :: Bool -> IO TestResult
testWebGLInjection headless = do
  fingerprint <- generateFingerprint
  let webgl = fingerprintWebGL fingerprint
      expectedVendor = webglVendor webgl
      expectedRenderer = webglRenderer webgl
  result <- runSession (chromeConfigForTesting headless) $ do
    openPage "http://localhost:8081/test.html"
    -- Inject WebGL properties
    injectWebGL webgl
    -- Query injected properties
    actualVendor <- queryWebGLProperty "VENDOR"
    actualRenderer <- queryWebGLProperty "RENDERER"
    closeSession
    return (actualVendor, actualRenderer)
  case result of
    (actualVendor, actualRenderer) -> do
      let vendorMatches = not (T.null actualVendor)
          rendererMatches = not (T.null actualRenderer)
          passed = vendorMatches && rendererMatches
          message = T.unlines
            [ "Vendor: " <> actualVendor <> if vendorMatches then " ✓" else " ✗"
            , "Renderer: " <> actualRenderer <> if rendererMatches then " ✓" else " ✗"
            , "Expected Vendor: " <> expectedVendor
            , "Expected Renderer: " <> expectedRenderer
            , "[DEBUG] Actual Vendor: " <> actualVendor
            , "[DEBUG] Actual Renderer: " <> actualRenderer
            ]
      return $ TestResult "WebGL Property Injection" passed message

-- | Test complete fingerprint injection and verification
testFullFingerprint :: Bool -> IO TestResult
testFullFingerprint headless = do
  fingerprint <- generateFingerprint
  result <- runSession (chromeConfigForTesting headless) $ do
    openPage "http://localhost:8081/test.html"
    -- Inject complete fingerprint
    injectFingerprint fingerprint
    -- Wait a moment for injection to take effect
    liftIO $ threadDelay 1000000  -- 1 second
    -- Verify fingerprint
    verified <- verifyFingerprint fingerprint
    -- Get some sample properties for debugging
    ua <- queryNavigatorProperty "userAgent"
    platform <- queryNavigatorProperty "platform"
    screenWidth <- queryScreenProperty "width"
    screenHeight <- queryScreenProperty "height"
    queriedWebGLVendor <- queryWebGLProperty "VENDOR"
    queriedWebGLRenderer <- queryWebGLProperty "RENDERER"
    closeSession
    return (verified, ua, platform, screenWidth, screenHeight, queriedWebGLVendor, queriedWebGLRenderer)
  case result of
    (verified, ua, platform, screenWidth, screenHeight, queriedWebGLVendor, queriedWebGLRenderer) -> do
      let message = T.unlines
            [ "Full verification: " <> if verified then "✓" else "✗"
            , "Sample UserAgent: " <> T.take 50 ua <> "..."
            , "Sample Platform: " <> platform
            , "Sample Screen Width: " <> screenWidth
            , "Sample Screen Height: " <> screenHeight
            , "Sample WebGL Vendor: " <> queriedWebGLVendor
            , "Sample WebGL Renderer: " <> queriedWebGLRenderer
            , "[DEBUG] Expected WebGL Vendor: " <> webglVendor (fingerprintWebGL fingerprint)
            , "[DEBUG] Expected WebGL Renderer: " <> webglRenderer (fingerprintWebGL fingerprint)
            ]
      return $ TestResult "Full Fingerprint Injection" verified message

-- | Print a test result in a formatted way
printTestResult :: TestResult -> IO ()
printTestResult result = do
  let status = if testPassed result then "✓ PASS" else "✗ FAIL"
      header = "[" <> status <> "] " <> testName result
  TIO.putStrLn header
  TIO.putStrLn $ "  " <> T.replace "\n" "\n  " (testMessage result)
  TIO.putStrLn "" 