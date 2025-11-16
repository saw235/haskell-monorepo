{-# LANGUAGE OverloadedStrings #-}

-- | Main executable for running fingerprint injection tests
--
-- This demonstrates how to use the fingerprint-injector library with WebDriver,
-- similar to the nike-scraper application.
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import FingerprintInjector
import FingerprintTest
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (getLine)
import System.Random (randomIO)
import Test.WebDriver
import Test.WebDriver.Commands

-- | CLI Options
data Command
  = RunTests Bool Bool -- headless, keepBrowser
  | RunDemo Bool (Maybe Int)
  | ShowFingerprint (Maybe Int)
  | DebugBrowser Bool (Maybe Int)
  deriving (Show)

headlessFlag :: Parser Bool
headlessFlag =
  switch (long "headless" <> help "Run tests in headless mode")

keepBrowserFlag :: Parser Bool
keepBrowserFlag =
  switch (long "keep-browser" <> help "Keep browser open after tests for debugging")

cliParser :: Parser Command
cliParser =
  subparser
    ( command "demo" (info (RunDemo <$> headlessFlag <*> optional seedOption) (progDesc "Run a demo injection"))
        <> command "show-fingerprint" (info (ShowFingerprint <$> optional seedOption) (progDesc "Show a sample generated fingerprint"))
        <> command "debug-browser" (info (DebugBrowser <$> headlessFlag <*> optional seedOption) (progDesc "Open a browser for manual debugging and inject a fingerprint"))
    )
    <|> (RunTests <$> headlessFlag <*> keepBrowserFlag)

seedOption :: Parser Int
seedOption =
  option
    auto
    ( long "seed"
        <> metavar "SEED"
        <> help "Seed for fingerprint generation (integer)"
    )

optsParserInfo :: ParserInfo Command
optsParserInfo =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Fingerprint Injector Test Suite"
        <> header "fingerprint-injector-test - test browser fingerprint injection"
    )

main :: IO ()
main = do
  cmd <- execParser optsParserInfo
  print cmd -- Debug print to show parsed command and headless value
  case cmd of
    RunTests headless keepBrowser -> runTests headless keepBrowser
    RunDemo headless mSeed -> runDemo headless mSeed
    ShowFingerprint mSeed -> showSampleFingerprint mSeed
    DebugBrowser headless mSeed -> debugBrowser headless mSeed

-- | Print usage information (now handled by optparse-applicative)
printUsage :: IO ()
printUsage = do
  putStrLn "Fingerprint Injector Test Suite"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  fingerprint-injector-test [--headless]"
  putStrLn "  fingerprint-injector-test demo [--headless]"
  putStrLn "  fingerprint-injector-test show-fingerprint"
  putStrLn "  fingerprint-injector-test --help"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  fingerprint-injector-test                # Run tests with visible browser"
  putStrLn "  fingerprint-injector-test --headless     # Run tests in headless mode"
  putStrLn "  fingerprint-injector-test demo           # Run demo injection"
  putStrLn "  fingerprint-injector-test show-fingerprint # Show a sample fingerprint"

-- | Run the test suite
runTests :: Bool -> Bool -> IO ()
runTests headless keepBrowser = do
  putStrLn $ "Running tests in " ++ (if headless then "headless" else "visible") ++ " mode..."
  putStrLn "NOTE: Make sure Chrome/Chromium and chromedriver are installed!"
  putStrLn ""

  results <- runFingerprintTests headless keepBrowser

  let failedTests = filter (not . testPassed) results

  if null failedTests
    then do
      putStrLn "🎉 All tests passed!"
      exitSuccess
    else do
      putStrLn $ "❌ " ++ show (length failedTests) ++ " test(s) failed."
      exitFailure

-- | Run a demo of fingerprint injection
runDemo :: Bool -> Maybe Int -> IO ()
runDemo headless mSeed = do
  putStrLn "=== Fingerprint Injection Demo ==="
  putStrLn ""
  seed <- maybe randomIO return mSeed
  let fingerprint = generateFingerprintWithSeed seed defaultFingerprintOptions
  putStrLn $ "Seed used: " ++ show seed
  putStrLn "Generated fingerprint:"
  TIO.putStrLn $ formatFingerprint fingerprint
  putStrLn ""
  putStrLn "This fingerprint would be injected into a WebDriver session to make"
  putStrLn "the browser appear more like a real user browser."
  putStrLn ""
  putStrLn "Key benefits:"
  putStrLn "- Realistic User-Agent strings"
  putStrLn "- Consistent navigator properties"
  putStrLn "- Appropriate screen resolutions"
  putStrLn "- WebGL vendor/renderer info"
  putStrLn "- Patches WebDriver detection"

-- | Show a sample generated fingerprint
showSampleFingerprint :: Maybe Int -> IO ()
showSampleFingerprint mSeed = do
  putStrLn "=== Sample Generated Fingerprint ==="
  putStrLn ""
  seed <- maybe randomIO return mSeed
  let fingerprint = generateFingerprintWithSeed seed defaultFingerprintOptions
  putStrLn $ "Seed used: " ++ show seed
  TIO.putStrLn $ formatFingerprint fingerprint
  putStrLn ""
  putStrLn "This fingerprint was generated using the AntiFingerprinting library."
  putStrLn "It provides realistic browser characteristics that can be injected"
  putStrLn "into WebDriver sessions for web scraping and automation."

-- | Open a browser for manual debugging and inject a fingerprint
debugBrowser :: Bool -> Maybe Int -> IO ()
debugBrowser headless mSeed = do
  seed <- maybe randomIO return mSeed
  let fingerprint = generateFingerprintWithSeed seed defaultFingerprintOptions
  putStrLn $ "Seed used: " ++ show seed
  putStrLn "Injected fingerprint:"
  TIO.putStrLn $ formatFingerprint fingerprint
  runSession (chromeConfigForTesting headless) $ do
    openPage "https://amiunique.org/"
    -- Inject the fingerprint
    injectFingerprint fingerprint
    -- Print the injected values to the browser console
    let jsMsg =
          "console.log('[Injected Fingerprint]');"
            <> "console.log('UserAgent:', navigator.userAgent);"
            <> "console.log('Platform:', navigator.platform);"
            <> "console.log('Vendor:', navigator.vendor);"
            <> "try { var canvas = document.createElement('canvas'); var gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl'); if (gl) { var debugInfo = gl.getExtension('WEBGL_debug_renderer_info'); if (debugInfo) { console.log('WebGL Vendor:', gl.getParameter(debugInfo.UNMASKED_VENDOR_WEBGL)); console.log('WebGL Renderer:', gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL)); }}} catch (e) { console.log('WebGL not supported or error:', e); }"
    _ <- (executeJS [] jsMsg :: WD ())
    liftIO $ putStrLn "[DEBUG] Browser opened, fingerprint injected, and values printed to browser console. Press Enter to close the browser..."
    _ <- liftIO getLine
    closeSession
