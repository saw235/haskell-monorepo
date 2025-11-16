{-# LANGUAGE OverloadedStrings #-}

-- | AntiFingerprinting - A Haskell library for browser fingerprint generation
--
-- This library provides tools for generating realistic browser fingerprints
-- to help with web scraping and automation by making requests appear more
-- like real user browsers.
module AntiFingerprinting
  ( -- * Core Types
    module AntiFingerprinting.Types,

    -- * Fingerprint Generation
    generateFingerprint,
    generateFingerprintWithOptions,
    generateFingerprintWithSeed,

    -- * Options and Defaults
    defaultFingerprintOptions,

    -- * Utility Functions
    formatFingerprint,
  )
where

import AntiFingerprinting.Generator
import AntiFingerprinting.Types
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Format a fingerprint for human-readable display
formatFingerprint :: Fingerprint -> Text
formatFingerprint fp =
  T.unlines
    [ "=== Browser Fingerprint ===",
      "",
      "User Agent:",
      "  " <> userAgentString (fingerprintUserAgent fp),
      "  Browser: " <> T.pack (show (userAgentBrowser (fingerprintUserAgent fp))),
      "  OS: " <> T.pack (show (userAgentOperatingSystem (fingerprintUserAgent fp))),
      "  Device: " <> T.pack (show (userAgentDevice (fingerprintUserAgent fp))),
      "",
      "Navigator:",
      "  Platform: " <> navigatorPlatform (fingerprintNavigator fp),
      "  Language: " <> navigatorLanguage (fingerprintNavigator fp),
      "  Vendor: " <> navigatorVendor (fingerprintNavigator fp),
      "",
      "Screen:",
      "  Resolution: " <> T.pack (show (screenWidth (fingerprintScreen fp))) <> "x" <> T.pack (show (screenHeight (fingerprintScreen fp))),
      "  Device Pixel Ratio: " <> T.pack (show (screenDevicePixelRatio (fingerprintScreen fp))),
      "",
      "WebGL:",
      "  Vendor: " <> webglVendor (fingerprintWebGL fp),
      "  Renderer: " <> webglRenderer (fingerprintWebGL fp),
      "",
      "Headers:"
    ]
    <> T.unlines (formatHeaders (fingerprintHeaders fp))
  where
    formatHeaders headers = map formatHeader (Map.toList headers)
      where
        formatHeader (name, value) = "  " <> name <> ": " <> value
