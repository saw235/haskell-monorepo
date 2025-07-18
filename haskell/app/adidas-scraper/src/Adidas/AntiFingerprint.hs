{-# LANGUAGE OverloadedStrings #-}

module Adidas.AntiFingerprint (
    setupAntiFingerprinting,
    setupAntiFingerprintingWithProfile,
    randomizeUserAgent,
    patchNavigatorProperties,
    addRandomDelays,
    humanizeScrolling,
    mockComprehensiveFingerprint,
    mockComprehensiveFingerprintWithProfile
) where

import Test.WebDriver.Monad (WD)
import Test.WebDriver.Commands
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import Adidas.FingerprintConfig (FingerprintProfile(..), defaultProfile)
import Data.Maybe (listToMaybe, maybe)

-- | Comprehensive anti-fingerprinting setup (legacy - uses default profile)
setupAntiFingerprinting :: WD ()
setupAntiFingerprinting = setupAntiFingerprintingWithProfile defaultProfile

-- | Comprehensive anti-fingerprinting setup with specific profile
setupAntiFingerprintingWithProfile :: FingerprintProfile -> WD ()
setupAntiFingerprintingWithProfile profile = do
    -- Apply fingerprint patches immediately
    mockComprehensiveFingerprintWithProfile profile
    addRandomDelays

-- | Mock a comprehensive browser fingerprint based on Apify research (legacy - uses default profile)
mockComprehensiveFingerprint :: WD ()
mockComprehensiveFingerprint = mockComprehensiveFingerprintWithProfile defaultProfile

-- | Mock a comprehensive browser fingerprint with specific profile
mockComprehensiveFingerprintWithProfile :: FingerprintProfile -> WD ()
mockComprehensiveFingerprintWithProfile profile = do
    -- Basic webdriver detection bypass
    _ <- (executeJS [] "Object.defineProperty(navigator, 'webdriver', {get: () => undefined});" :: WD ())
    
    -- Mock realistic browser fingerprint using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'userAgent', {get: () => '" ++ T.unpack (userAgent profile) ++ "'});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'platform', {get: () => '" ++ T.unpack (platform profile) ++ "'});" :: WD ())
    _ <- (executeJS [] "Object.defineProperty(navigator, 'cookieEnabled', {get: () => true});" :: WD ())
    _ <- (executeJS [] "Object.defineProperty(navigator, 'doNotTrack', {get: () => '1'});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'productSub', {get: () => '" ++ T.unpack (productSub profile) ++ "'});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'hardwareConcurrency', {get: () => " ++ show (hardwareConcurrency profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'deviceMemory', {get: () => " ++ show (deviceMemory profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'languages', {get: () => ['" ++ T.unpack (T.intercalate "', '" (languages profile)) ++ "']});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'language', {get: () => '" ++ T.unpack (maybe "en-US" id (listToMaybe (languages profile))) ++ "'});" :: WD ())
    
    -- Mock plugins and MIME types with proper format
    let pluginsStr = T.intercalate ", " $ map (\(name, _) -> "'" <> name <> "'") (plugins profile)
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'plugins', {get: () => [" ++ T.unpack pluginsStr ++ "].map(name => ({name: name, filename: 'internal-pdf-viewer'}))});" :: WD ())
    
    let mimeTypesStr = T.intercalate ", " $ map (\(type_, _) -> "'" <> type_ <> "'") (mimeTypes profile)
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'mimeTypes', {get: () => [" ++ T.unpack mimeTypesStr ++ "].map(type => ({type: type, description: 'Portable Document Format'}))});" :: WD ())
    
    -- Mock vendor and product info using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'vendor', {get: () => '" ++ T.unpack (vendor profile) ++ "'});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'product', {get: () => '" ++ T.unpack (browserProduct profile) ++ "'});" :: WD ())
    
    -- Mock connection info using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'connection', {get: () => ({downlink: " ++ show (connectionDownlink profile) ++ ", effectiveType: '" ++ T.unpack (connectionType profile) ++ "', rtt: 50})});" :: WD ())
    
    -- Mock touch support using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'maxTouchPoints', {get: () => " ++ show (maxTouchPoints profile) ++ "});" :: WD ())
    
    -- Mock screen properties using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'width', {get: () => " ++ show (screenWidth profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'height', {get: () => " ++ show (screenHeight profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'availWidth', {get: () => " ++ show (screenWidth profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'availHeight', {get: () => " ++ show (screenHeight profile - 40) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'colorDepth', {get: () => " ++ show (colorDepth profile) ++ "});" :: WD ())
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(screen, 'pixelDepth', {get: () => " ++ show (colorDepth profile) ++ "});" :: WD ())
    
    -- Mock window properties
    _ <- (executeJS [] "Object.defineProperty(window, 'outerWidth', {get: () => window.innerWidth});" :: WD ())
    _ <- (executeJS [] "Object.defineProperty(window, 'outerHeight', {get: () => window.innerHeight});" :: WD ())
    
    -- Mock timezone using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(Intl, 'DateTimeFormat', {get: () => function() { return {resolvedOptions: () => ({timeZone: '" ++ T.unpack (timezone profile) ++ "'})}; }});" :: WD ())
    
    -- Mock audio codec support using profile
    let audioCodecsStr = T.intercalate ", " $ map (\(codec, support) -> "'" <> codec <> "': '" <> support <> "'") (audioCodecs profile)
    _ <- (executeJS [] $ T.pack $ "HTMLMediaElement.prototype.canPlayType = function(type) { const codecs = {" ++ T.unpack audioCodecsStr ++ "}; return codecs[type] || ''; };" :: WD ())
    
    -- Mock video codec support using profile
    let videoCodecsStr = T.intercalate ", " $ map (\(codec, support) -> "'" <> codec <> "': '" <> support <> "'") (videoCodecs profile)
    _ <- (executeJS [] $ T.pack $ "HTMLVideoElement.prototype.canPlayType = function(type) { const codecs = {" ++ T.unpack videoCodecsStr ++ "}; return codecs[type] || ''; };" :: WD ())
    
    -- Mock WebGL fingerprinting using profile
    _ <- (executeJS [] $ T.pack $ "const getParameter = WebGLRenderingContext.prototype.getParameter; WebGLRenderingContext.prototype.getParameter = function(parameter) { if (parameter === 37445) return '" ++ T.unpack (webglVendor profile) ++ "'; if (parameter === 37446) return '" ++ T.unpack (webglRenderer profile) ++ "'; return getParameter.call(this, parameter); };" :: WD ())
    
    -- Mock canvas fingerprinting
    _ <- (executeJS [] "const getImageData = CanvasRenderingContext2D.prototype.getImageData; CanvasRenderingContext2D.prototype.getImageData = function() { const imageData = getImageData.apply(this, arguments); const data = imageData.data; for (let i = 0; i < data.length; i += 4) { data[i] = data[i] + Math.random() * 0.01; } return imageData; };" :: WD ())
    
    -- Mock AudioContext fingerprinting
    _ <- (executeJS [] "const getChannelData = AudioBuffer.prototype.getChannelData; AudioBuffer.prototype.getChannelData = function() { const channelData = getChannelData.apply(this, arguments); for (let i = 0; i < channelData.length; i++) { channelData[i] = channelData[i] + Math.random() * 0.0001; } return channelData; };" :: WD ())
    
    -- Mock permissions
    _ <- (executeJS [] "navigator.permissions.query = (p) => Promise.resolve({state: 'granted'});" :: WD ())
    
    -- Mock media devices with detailed fingerprint using profile
    let (speakers, micros, webcams) = multimediaDevices profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'mediaDevices', {get: () => ({enumerateDevices: () => Promise.resolve([{deviceId: 'speaker-1', kind: 'audiooutput', label: 'Default - Speakers'}, {deviceId: 'micro-1', kind: 'audioinput', label: 'Default - Microphone'}, {deviceId: 'camera-1', kind: 'videoinput', label: 'Default - Webcam'}])})});" :: WD ())
    
    -- Mock multimedia devices count using profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(navigator, 'multimediaDevices', {get: () => ({speakers: " ++ show speakers ++ ", micros: " ++ show micros ++ ", webcams: " ++ show webcams ++ "})});" :: WD ())
    
    -- Mock tpCanvas fingerprint using profile
    let (tp0, tp1, tp2, tp3) = tpCanvas profile
    _ <- (executeJS [] $ T.pack $ "Object.defineProperty(window, 'tpCanvas', {get: () => ({'0': " ++ show tp0 ++ ", '1': " ++ show tp1 ++ ", '2': " ++ show tp2 ++ ", '3': " ++ show tp3 ++ "})});" :: WD ())
    
    -- Mock gamepads and VR
    _ <- (executeJS [] "navigator.getGamepads = () => [];" :: WD ())
    _ <- (executeJS [] "navigator.getVRDisplays = () => [];" :: WD ())
    
    -- Mock battery API
    _ <- (executeJS [] "navigator.getBattery = () => Promise.resolve({charging: true, chargingTime: 0, dischargingTime: Infinity, level: 0.8});" :: WD ())
    
    -- Mock user activation
    _ <- (executeJS [] "Object.defineProperty(navigator, 'userActivation', {get: () => ({hasBeenActive: true, isActive: false})});" :: WD ())
    
    -- Mock chrome runtime
    _ <- (executeJS [] "window.chrome = {runtime: {}, loadTimes: function() {}, csi: function() {}, app: {}};" :: WD ())
    
    -- Mock Notification permission
    _ <- (executeJS [] "Object.defineProperty(window.Notification, 'permission', {get: () => 'default'});" :: WD ())
    
    -- Remove automation indicators
    _ <- (executeJS [] "delete window.cdc_adoQpoasnfa76pfcZLmcfl_Array;" :: WD ())
    _ <- (executeJS [] "delete window.cdc_adoQpoasnfa76pfcZLmcfl_Promise;" :: WD ())
    _ <- (executeJS [] "delete window.cdc_adoQpoasnfa76pfcZLmcfl_Symbol;" :: WD ())
    
    -- Mock userAgentData (Chrome 90+)
    _ <- (executeJS [] "Object.defineProperty(navigator, 'userAgentData', {get: () => ({brands: [{brand: 'Google Chrome', version: '137'}, {brand: 'Chromium', version: '137'}], mobile: false, platform: 'Linux'})});" :: WD ())
    
    pure ()

-- | Randomize User-Agent to avoid detection
randomizeUserAgent :: WD String
randomizeUserAgent = do
    userAgents <- liftIO $ return [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
                                 , "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
                                 , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
                                 , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.1 Safari/605.1.15"
                                 , "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
                                 , "Mozilla/5.0 (X11; Linux x86_64; rv:90.0) Gecko/20100101 Firefox/90.0"
                                 ]
    selectedAgent <- liftIO $ (userAgents !!) <$> randomRIO (0, length userAgents - 1)
    return selectedAgent

-- | Patch navigator properties to avoid fingerprinting (legacy function)
patchNavigatorProperties :: WD ()
patchNavigatorProperties = mockComprehensiveFingerprint

-- | Add random delays to simulate human behavior
addRandomDelays :: WD ()
addRandomDelays = do
    delay <- liftIO $ randomRIO (100, 500)
    liftIO $ threadDelay (delay * 1000)
    pure ()

-- | Humanize scrolling behavior
humanizeScrolling :: WD ()
humanizeScrolling = do
    -- Random scroll amount
    scrollAmount <- liftIO $ randomRIO (300 :: Int, 800 :: Int)
    _ <- (executeJS [] $ T.pack $ "window.scrollBy(0, " ++ show scrollAmount ++ ");" :: WD ())
    
    -- Random delay after scroll
    delay <- liftIO $ randomRIO (500 :: Int, 2000 :: Int)
    liftIO $ threadDelay (delay * 1000)
    pure () 