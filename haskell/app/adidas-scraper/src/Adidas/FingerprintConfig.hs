{-# LANGUAGE OverloadedStrings #-}

module Adidas.FingerprintConfig
  ( FingerprintProfile (..),
    defaultProfile,
    firefoxProfile,
    chromeProfile,
    safariProfile,
    getFingerprintConfig,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | Browser fingerprint configuration profile
data FingerprintProfile = FingerprintProfile
  { userAgent :: Text,
    platform :: Text,
    vendor :: Text,
    browserProduct :: Text,
    productSub :: Text,
    hardwareConcurrency :: Int,
    deviceMemory :: Int,
    languages :: [Text],
    screenWidth :: Int,
    screenHeight :: Int,
    colorDepth :: Int,
    timezone :: Text,
    webglVendor :: Text,
    webglRenderer :: Text,
    audioCodecs :: [(Text, Text)],
    videoCodecs :: [(Text, Text)],
    plugins :: [(Text, Text)],
    mimeTypes :: [(Text, Text)],
    multimediaDevices :: (Int, Int, Int), -- (speakers, micros, webcams)
    tpCanvas :: (Int, Int, Int, Int), -- (0, 1, 2, 3)
    connectionType :: Text,
    connectionDownlink :: Int,
    maxTouchPoints :: Int
  }

-- | Default fingerprint profile (Firefox on Linux)
defaultProfile :: FingerprintProfile
defaultProfile =
  FingerprintProfile
    { userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:90.0) Gecko/20100101 Firefox/90.0",
      platform = "Linux x86_64",
      vendor = "Google Inc.",
      browserProduct = "Gecko",
      productSub = "20100101",
      hardwareConcurrency = 8,
      deviceMemory = 8,
      languages = ["en-US", "en"],
      screenWidth = 1920,
      screenHeight = 1080,
      colorDepth = 24,
      timezone = "Europe/Prague",
      webglVendor = "Intel Open Source Technology Center",
      webglRenderer = "Mesa DRI Intel(R) HD Graphics 4600 (HSW GT2)",
      audioCodecs =
        [ ("audio/ogg", "probably"),
          ("audio/mp3", "maybe"),
          ("audio/wav", "probably"),
          ("audio/m4a", "maybe"),
          ("audio/aac", "maybe")
        ],
      videoCodecs =
        [ ("video/ogg", "probably"),
          ("video/h264", "probably"),
          ("video/webm", "probably")
        ],
      plugins = [("VKkaVKs1::T05FCozZz4k5cOmyCBn6laNl5kx3bVKk::DozZz4cOuf2bNteP::__~q0i~DJMtWyhQQvAny4k5kx3j47d1asWq8Hi4", "internal-pdf-viewer"), ("JavaScript Portable Document Format Viewer::Portable Document Format::QQnTRQv2Epc1iZUxgvXTwBIr0a0DgYz4::__application/x-google-chrome-pdf~pdf~Portable Document Format", "internal-pdf-viewer"), ("NtWq0a0::z4cOuXToUSRQvAny4kx3j4FCgQIMlx3::8DBn6laVKs9e2jw::__~FOP~FCgQIMlxBIr0a0DgYrVxYMGi4FKFhvfu", "internal-pdf-viewer"), ("Web com.adobe.pdf Renderer::::d1a0DgYrdOufuAny4kxBIr0asWq0asWy::__application/pdf~pdf~", "internal-pdf-viewer")],
      mimeTypes = [("~~application/pdf~~pdf", "Portable Document Format"), ("Portable Document Format~~application/x-google-chrome-pdf~~pdf", "Portable Document Format")],
      multimediaDevices = (1, 1, 1), -- (speakers, micros, webcams)
      tpCanvas = (0, 1, 1, 0), -- (0, 1, 2, 3)
      connectionType = "4g",
      connectionDownlink = 10,
      maxTouchPoints = 0
    }

-- | Firefox-specific profile
firefoxProfile :: FingerprintProfile
firefoxProfile =
  defaultProfile
    { userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:120.0) Gecko/20100101 Firefox/120.0",
      browserProduct = "Gecko",
      productSub = "20100101",
      vendor = "Google Inc."
    }

-- | Chrome-specific profile
chromeProfile :: FingerprintProfile
chromeProfile =
  FingerprintProfile
    { userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36",
      platform = "Linux x86_64",
      vendor = "Google Inc.",
      browserProduct = "Gecko",
      productSub = "20030107",
      hardwareConcurrency = 8,
      deviceMemory = 8,
      languages = ["en-US", "en"],
      screenWidth = 1920,
      screenHeight = 1080,
      colorDepth = 24,
      timezone = "America/New_York",
      webglVendor = "Google Inc. (Microsoft Corporation)",
      webglRenderer = "ANGLE (Microsoft Corporation, D3D12 (AMD Radeon RX 7900 XT), OpenGL 4.6)",
      audioCodecs =
        [ ("audio/ogg", "probably"),
          ("audio/mp3", "probably"),
          ("audio/wav", "probably"),
          ("audio/m4a", "maybe"),
          ("audio/aac", "probably")
        ],
      videoCodecs =
        [ ("video/ogg", ""),
          ("video/h264", "probably"),
          ("video/webm", "probably")
        ],
      plugins =
        [ ("PDF Viewer::Portable Document Format::internal-pdf-viewer::__application/pdf~pdf~Portable Document Format,text/pdf~pdf~Portable Document Format", "internal-pdf-viewer"),
          ("Chrome PDF Viewer::Portable Document Format::internal-pdf-viewer::__application/pdf~pdf~Portable Document Format,text/pdf~pdf~Portable Document Format", "internal-pdf-viewer"),
          ("Chromium PDF Viewer::Portable Document Format::internal-pdf-viewer::__application/pdf~pdf~Portable Document Format,text/pdf~pdf~Portable Document Format", "internal-pdf-viewer"),
          ("Microsoft Edge PDF Viewer::Portable Document Format::internal-pdf-viewer::__application/pdf~pdf~Portable Document Format,text/pdf~pdf~Portable Document Format", "internal-pdf-viewer"),
          ("WebKit built-in PDF::Portable Document Format::internal-pdf-viewer::__application/pdf~pdf~Portable Document Format,text/pdf~pdf~Portable Document Format", "internal-pdf-viewer")
        ],
      mimeTypes =
        [ ("Portable Document Format~~application/pdf~~pdf", "Portable Document Format"),
          ("Portable Document Format~~text/pdf~~pdf", "Portable Document Format")
        ],
      multimediaDevices = (1, 1, 1), -- (speakers, micros, webcams) - match the page result
      tpCanvas = (0, 1, 1, 0), -- (0, 1, 2, 3) - match the page result
      connectionType = "4g",
      connectionDownlink = 15,
      maxTouchPoints = 0
    }

-- | Safari-specific profile
safariProfile :: FingerprintProfile
safariProfile =
  FingerprintProfile
    { userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.1 Safari/605.1.15",
      platform = "MacIntel",
      vendor = "Apple Computer, Inc.",
      browserProduct = "Gecko",
      productSub = "605.1.15",
      hardwareConcurrency = 8,
      deviceMemory = 8,
      languages = ["en-US", "en"],
      screenWidth = 1440,
      screenHeight = 900,
      colorDepth = 24,
      timezone = "America/Los_Angeles",
      webglVendor = "Apple Inc.",
      webglRenderer = "Apple M1 Pro",
      audioCodecs =
        [ ("audio/ogg", "maybe"),
          ("audio/mp3", "probably"),
          ("audio/wav", "probably"),
          ("audio/m4a", "probably"),
          ("audio/aac", "probably")
        ],
      videoCodecs =
        [ ("video/ogg", "maybe"),
          ("video/h264", "probably"),
          ("video/webm", "maybe"),
          ("video/mp4", "probably")
        ],
      plugins = [("Web com.adobe.pdf Renderer::::d1a0DgYrdOufuAny4kxBIr0asWq0asWy::__application/pdf~pdf~", "internal-pdf-viewer")],
      mimeTypes = [("~~application/pdf~~pdf", "Portable Document Format")],
      multimediaDevices = (1, 1, 1), -- (speakers, micros, webcams)
      tpCanvas = (0, 1, 1, 0), -- (0, 1, 2, 3)
      connectionType = "4g",
      connectionDownlink = 12,
      maxTouchPoints = 0
    }

-- | Get fingerprint configuration as JavaScript object
getFingerprintConfig :: FingerprintProfile -> Text
getFingerprintConfig profile =
  T.unwords
    [ "{",
      "userAgent: '" <> userAgent profile <> "',",
      "platform: '" <> platform profile <> "',",
      "vendor: '" <> vendor profile <> "',",
      "product: '" <> browserProduct profile <> "',",
      "productSub: '" <> productSub profile <> "',",
      "hardwareConcurrency: " <> T.pack (show $ hardwareConcurrency profile) <> ",",
      "deviceMemory: " <> T.pack (show $ deviceMemory profile) <> ",",
      "languages: ['" <> T.intercalate "', '" (languages profile) <> "'],",
      "screenWidth: " <> T.pack (show $ screenWidth profile) <> ",",
      "screenHeight: " <> T.pack (show $ screenHeight profile) <> ",",
      "colorDepth: " <> T.pack (show $ colorDepth profile) <> ",",
      "timezone: '" <> timezone profile <> "',",
      "webglVendor: '" <> webglVendor profile <> "',",
      "webglRenderer: '" <> webglRenderer profile <> "',",
      "plugins: ['" <> T.intercalate "', '" (map fst $ plugins profile) <> "'],",
      "mimeTypes: ['" <> T.intercalate "', '" (map fst $ mimeTypes profile) <> "'],",
      "multimediaDevices: {speakers: " <> T.pack (show $ (\(s, _, _) -> s) $ multimediaDevices profile) <> ", micros: " <> T.pack (show $ (\(_, m, _) -> m) $ multimediaDevices profile) <> ", webcams: " <> T.pack (show $ (\(_, _, w) -> w) $ multimediaDevices profile) <> "},",
      "tpCanvas: {'0': " <> T.pack (show $ (\(a, _, _, _) -> a) $ tpCanvas profile) <> ", '1': " <> T.pack (show $ (\(_, b, _, _) -> b) $ tpCanvas profile) <> ", '2': " <> T.pack (show $ (\(_, _, c, _) -> c) $ tpCanvas profile) <> ", '3': " <> T.pack (show $ (\(_, _, _, d) -> d) $ tpCanvas profile) <> "},",
      "connectionType: '" <> connectionType profile <> "',",
      "connectionDownlink: " <> T.pack (show $ connectionDownlink profile) <> ",",
      "maxTouchPoints: " <> T.pack (show $ maxTouchPoints profile),
      "}"
    ]
