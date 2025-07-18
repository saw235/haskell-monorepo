{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.Monad (WD)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Adidas.AntiFingerprint (mockComprehensiveFingerprintWithProfile)
import qualified Adidas.FingerprintConfig as FC
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, maybe)

-- | Configuration for Chrome WebDriver with anti-fingerprinting
chromeConfig :: Bool -> WDConfig
chromeConfig headless = useBrowser (chrome { chromeOptions = opts }) defaultConfig
  where
    -- Enhanced anti-fingerprinting options
    baseOpts = [ "--no-sandbox"
               , "--disable-dev-shm-usage"
               , "--disable-blink-features=AutomationControlled"
               , "--disable-extensions"
               , "--disable-plugins-discovery"
               , "--disable-default-apps"
               , "--disable-sync"
               , "--disable-translate"
               , "--disable-background-timer-throttling"
               , "--disable-backgrounding-occluded-windows"
               , "--disable-renderer-backgrounding"
               , "--disable-features=TranslateUI"
               , "--disable-ipc-flooding-protection"
               , "--no-first-run"
               , "--no-default-browser-check"
               , "--disable-web-security"
               , "--disable-features=VizDisplayCompositor"
               ]
    headlessOpts = if headless then ["--headless"] else ["--start-minimized"]
    opts = headlessOpts ++ baseOpts

-- Helper for reporting mismatches
reportMismatch :: (Eq a, Show a) => String -> a -> a -> [String]
reportMismatch label actual expected = if actual == expected then [] else [label ++ ": actual=" ++ show actual ++ ", expected=" ++ show expected]

-- | Test fingerprint configuration with comprehensive debugging
-- Only print mismatches, collect all and print summary at end

testFingerprintComprehensive :: FC.FingerprintProfile -> String -> WD ()
testFingerprintComprehensive profile profileName = do
    liftIO $ putStrLn $ "🔧 Testing " ++ profileName ++ " Fingerprint Configuration..."
    openPage "about:blank"
    mockComprehensiveFingerprintWithProfile profile
    liftIO $ putStrLn "✅ Fingerprint patches applied on blank page"

    let expect = profile
    mismatches <- fmap concat $ sequence
      [ do ua <- executeJS [] "return navigator.userAgent;" :: WD T.Text
           return $ reportMismatch "userAgent" ua (FC.userAgent expect)
      , do platform <- executeJS [] "return navigator.platform;" :: WD T.Text
           return $ reportMismatch "platform" platform (FC.platform expect)
      , do productSub <- executeJS [] "return navigator.productSub;" :: WD T.Text
           return $ reportMismatch "productSub" productSub (FC.productSub expect)
      , do hardwareConcurrency <- executeJS [] "return navigator.hardwareConcurrency;" :: WD Int
           return $ reportMismatch "hardwareConcurrency" hardwareConcurrency (FC.hardwareConcurrency expect)
      , do deviceMemory <- executeJS [] "return navigator.deviceMemory;" :: WD Int
           return $ reportMismatch "deviceMemory" deviceMemory (FC.deviceMemory expect)
      , do vendor <- executeJS [] "return navigator.vendor;" :: WD T.Text
           return $ reportMismatch "vendor" vendor (FC.vendor expect)
      , do product <- executeJS [] "return navigator.product;" :: WD T.Text
           return $ reportMismatch "product" product (FC.browserProduct expect)
      , do languages <- executeJS [] "return navigator.languages;" :: WD [T.Text]
           return $ reportMismatch "languages" languages (FC.languages expect)
      , do language <- executeJS [] "return navigator.language;" :: WD T.Text
           return $ reportMismatch "language" language (maybe "en-US" id (listToMaybe (FC.languages expect)))
      , do plugins <- executeJS [] "return Array.from(navigator.plugins).map(p => p.name);" :: WD [T.Text]
           return $ reportMismatch "plugins" plugins (map fst (FC.plugins expect))
      , do mimeTypes <- executeJS [] "return Array.from(navigator.mimeTypes).map(m => m.type);" :: WD [T.Text]
           return $ reportMismatch "mimeTypes" mimeTypes (map fst (FC.mimeTypes expect))
      , do screenWidth <- executeJS [] "return screen.width;" :: WD Int
           return $ reportMismatch "screen.width" screenWidth (FC.screenWidth expect)
      , do screenHeight <- executeJS [] "return screen.height;" :: WD Int
           return $ reportMismatch "screen.height" screenHeight (FC.screenHeight expect)
      , do availWidth <- executeJS [] "return screen.availWidth;" :: WD Int
           return $ reportMismatch "screen.availWidth" availWidth (FC.screenWidth expect)
      , do availHeight <- executeJS [] "return screen.availHeight;" :: WD Int
           return $ reportMismatch "screen.availHeight" availHeight (FC.screenHeight expect - 40)
      , do colorDepth <- executeJS [] "return screen.colorDepth;" :: WD Int
           return $ reportMismatch "screen.colorDepth" colorDepth (FC.colorDepth expect)
      , do pixelDepth <- executeJS [] "return screen.pixelDepth;" :: WD Int
           return $ reportMismatch "screen.pixelDepth" pixelDepth (FC.colorDepth expect)
      , do speakers <- executeJS [] "return navigator.multimediaDevices ? navigator.multimediaDevices.speakers : null;" :: WD Int
           let (s,_,_) = FC.multimediaDevices expect in return $ reportMismatch "multimediaDevices.speakers" speakers s
      , do micros <- executeJS [] "return navigator.multimediaDevices ? navigator.multimediaDevices.micros : null;" :: WD Int
           let (_,m,_) = FC.multimediaDevices expect in return $ reportMismatch "multimediaDevices.micros" micros m
      , do webcams <- executeJS [] "return navigator.multimediaDevices ? navigator.multimediaDevices.webcams : null;" :: WD Int
           let (_,_,w) = FC.multimediaDevices expect in return $ reportMismatch "multimediaDevices.webcams" webcams w
      , do tp0 <- executeJS [] "return window.tpCanvas ? window.tpCanvas['0'] : null;" :: WD Int
           let (a,_,_,_) = FC.tpCanvas expect in return $ reportMismatch "tpCanvas.0" tp0 a
      , do tp1 <- executeJS [] "return window.tpCanvas ? window.tpCanvas['1'] : null;" :: WD Int
           let (_,b,_,_) = FC.tpCanvas expect in return $ reportMismatch "tpCanvas.1" tp1 b
      , do tp2 <- executeJS [] "return window.tpCanvas ? window.tpCanvas['2'] : null;" :: WD Int
           let (_,_,c,_) = FC.tpCanvas expect in return $ reportMismatch "tpCanvas.2" tp2 c
      , do tp3 <- executeJS [] "return window.tpCanvas ? window.tpCanvas['3'] : null;" :: WD Int
           let (_,_,_,d) = FC.tpCanvas expect in return $ reportMismatch "tpCanvas.3" tp3 d
      , do connDownlink <- executeJS [] "return navigator.connection ? navigator.connection.downlink : null;" :: WD Int
           return $ reportMismatch "connection.downlink" connDownlink (FC.connectionDownlink expect)
      , do connType <- executeJS [] "return navigator.connection ? navigator.connection.effectiveType : null;" :: WD T.Text
           return $ reportMismatch "connection.effectiveType" connType (FC.connectionType expect)
      , do maxTouchPoints <- executeJS [] "return navigator.maxTouchPoints;" :: WD Int
           return $ reportMismatch "maxTouchPoints" maxTouchPoints (FC.maxTouchPoints expect)
      , do timezone <- executeJS [] "return Intl.DateTimeFormat().resolvedOptions().timeZone;" :: WD T.Text
           return $ reportMismatch "timezone" timezone (FC.timezone expect)
      , do webglVendor <- executeJS [] "return (function(){try{var c=document.createElement('canvas').getContext('webgl');return c.getParameter(37445);}catch(e){return null;}})();" :: WD T.Text
           return $ reportMismatch "webglVendor" webglVendor (FC.webglVendor expect)
      , do webglRenderer <- executeJS [] "return (function(){try{var c=document.createElement('canvas').getContext('webgl');return c.getParameter(37446);}catch(e){return null;}})();" :: WD T.Text
           return $ reportMismatch "webglRenderer" webglRenderer (FC.webglRenderer expect)
      , do audioOgg <- executeJS [] "return document.createElement('audio').canPlayType('audio/ogg');" :: WD T.Text
           return $ reportMismatch "audio/ogg" audioOgg (maybe "" id (lookup "audio/ogg" (FC.audioCodecs expect)))
      , do audioMp3 <- executeJS [] "return document.createElement('audio').canPlayType('audio/mp3');" :: WD T.Text
           return $ reportMismatch "audio/mp3" audioMp3 (maybe "" id (lookup "audio/mp3" (FC.audioCodecs expect)))
      , do audioWav <- executeJS [] "return document.createElement('audio').canPlayType('audio/wav');" :: WD T.Text
           return $ reportMismatch "audio/wav" audioWav (maybe "" id (lookup "audio/wav" (FC.audioCodecs expect)))
      , do audioM4a <- executeJS [] "return document.createElement('audio').canPlayType('audio/m4a');" :: WD T.Text
           return $ reportMismatch "audio/m4a" audioM4a (maybe "" id (lookup "audio/m4a" (FC.audioCodecs expect)))
      , do audioAac <- executeJS [] "return document.createElement('audio').canPlayType('audio/aac');" :: WD T.Text
           return $ reportMismatch "audio/aac" audioAac (maybe "" id (lookup "audio/aac" (FC.audioCodecs expect)))
      , do videoOgg <- executeJS [] "return document.createElement('video').canPlayType('video/ogg');" :: WD T.Text
           return $ reportMismatch "video/ogg" videoOgg (maybe "" id (lookup "video/ogg" (FC.videoCodecs expect)))
      , do videoH264 <- executeJS [] "return document.createElement('video').canPlayType('video/h264');" :: WD T.Text
           return $ reportMismatch "video/h264" videoH264 (maybe "" id (lookup "video/h264" (FC.videoCodecs expect)))
      , do videoWebm <- executeJS [] "return document.createElement('video').canPlayType('video/webm');" :: WD T.Text
           return $ reportMismatch "video/webm" videoWebm (maybe "" id (lookup "video/webm" (FC.videoCodecs expect)))
      ]
    if null mismatches
      then liftIO $ putStrLn "[CHECK] All fingerprint properties match expected profile."
      else do
        liftIO $ putStrLn "[CHECK] Mismatches found:"
        mapM_ (liftIO . putStrLn) mismatches
    
    -- Now navigate to fingerprint test site with patches already in place
    openPage "https://bot.sannysoft.com"
    
    -- Wait for page to load
    liftIO $ threadDelay (3000000) -- 3 seconds
    
    -- Get page title to confirm it loaded
    title <- getTitle
    liftIO $ putStrLn $ "📄 Page title: " ++ T.unpack title
    
    -- Execute JavaScript to check what values are actually set
    _ <- executeJS [] "console.log('=== FINGERPRINT DEBUG INFO ===');" :: WD ()
    _ <- executeJS [] "console.log('User Agent:', navigator.userAgent || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Product Sub:', navigator.productSub || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Platform:', navigator.platform || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Vendor:', navigator.vendor || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Product:', navigator.product || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Hardware Concurrency:', navigator.hardwareConcurrency || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Device Memory:', navigator.deviceMemory || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Languages:', navigator.languages || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Plugins:', Array.from(navigator.plugins || []).map(p => p.name).join(', ') || 'null');" :: WD ()
    _ <- executeJS [] "console.log('MIME Types:', Array.from(navigator.mimeTypes || []).map(m => m.type).join(', ') || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Multimedia Devices:', navigator.multimediaDevices ? JSON.stringify(navigator.multimediaDevices) : 'null');" :: WD ()
    _ <- executeJS [] "console.log('TP Canvas:', window.tpCanvas ? JSON.stringify(window.tpCanvas) : 'null');" :: WD ()
    _ <- executeJS [] "console.log('WebDriver:', navigator.webdriver || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Screen Width:', screen.width || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Screen Height:', screen.height || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Color Depth:', screen.colorDepth || 'null');" :: WD ()
    _ <- executeJS [] "console.log('Connection:', navigator.connection ? JSON.stringify(navigator.connection) : 'null');" :: WD ()
    _ <- executeJS [] "console.log('Max Touch Points:', navigator.maxTouchPoints || 'null');" :: WD ()
    _ <- executeJS [] "console.log('=== END FINGERPRINT DEBUG INFO ===');" :: WD ()
    
    liftIO $ putStrLn "📊 Check browser console for detailed fingerprint values"
    liftIO $ putStrLn "🔍 Manual verification:"
    liftIO $ putStrLn "   • Check browser window for fingerprint test results"
    liftIO $ putStrLn "   • Green indicators = Good (not detected)"
    liftIO $ putStrLn "   • Red indicators = Detected (needs improvement)"
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Press Enter to continue..."
    _ <- liftIO getLine
    pure ()

-- | Main test function
runFingerprintTests :: WD ()
runFingerprintTests = do
    liftIO $ putStrLn "🚀 Starting Comprehensive Fingerprint Tests"
    liftIO $ putStrLn "============================================="
    
    -- Test Chrome profile
    testFingerprintComprehensive FC.chromeProfile "Chrome"
    
    -- Test Firefox profile
    testFingerprintComprehensive FC.firefoxProfile "Firefox"
    
    -- Test Safari profile
    testFingerprintComprehensive FC.safariProfile "Safari"
    
    -- Test Default profile
    testFingerprintComprehensive FC.defaultProfile "Default"
    
    liftIO $ putStrLn "✅ All fingerprint tests completed!"

main :: IO ()
main = do
    args <- getArgs
    let headless = False -- Always run in non-headless mode for testing
    
    putStrLn "🧪 Adidas Comprehensive Fingerprint Test Suite"
    putStrLn "=============================================="
    putStrLn "This test will run all fingerprint profiles and show detailed debugging info."
    putStrLn "Check the browser console for detailed fingerprint values."
    putStrLn ""
    
    -- Run the comprehensive test
    runSession (chromeConfig headless) runFingerprintTests
    
    putStrLn ""
    putStrLn "🎯 Test Summary:"
    putStrLn "   • All fingerprint profiles tested"
    putStrLn "   • Enhanced patching applied"
    putStrLn "   • Detailed debugging information logged to console"
    putStrLn "   • Manual verification completed"
    putStrLn ""
    putStrLn "📋 Next Steps:"
    putStrLn "   • Review console output for fingerprint values"
    putStrLn "   • Check browser for detection results"
    putStrLn "   • Adjust fingerprint profiles if needed" 