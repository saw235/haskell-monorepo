{-# LANGUAGE OverloadedStrings #-}

module Adidas.ProxyManager (
    ProxyConfig(..),
    SessionConfig(..),
    rotateProxy,
    createSessionWithProxy,
    getRandomProxy,
    validateProxy
) where

import Test.WebDriver
import Test.WebDriver.Monad (WD)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

-- | Proxy configuration
data ProxyConfig = ProxyConfig
    { proxyHost :: String
    , proxyPort :: Int
    , proxyUsername :: Maybe String
    , proxyPassword :: Maybe String
    } deriving (Show, Eq)

-- | Session configuration with proxy
data SessionConfig = SessionConfig
    { useProxy :: Bool
    , proxyList :: [ProxyConfig]
    , sessionTimeout :: Int
    , maxRetries :: Int
    } deriving (Show)

-- | Default proxy list (you should replace these with real proxies)
defaultProxies :: [ProxyConfig]
defaultProxies = [
    ProxyConfig "proxy1.example.com" 8080 Nothing Nothing,
    ProxyConfig "proxy2.example.com" 8080 Nothing Nothing,
    ProxyConfig "proxy3.example.com" 8080 Nothing Nothing
    ]

-- | Get a random proxy from the list
getRandomProxy :: [ProxyConfig] -> IO (Maybe ProxyConfig)
getRandomProxy [] = return Nothing
getRandomProxy proxies = do
    index <- randomRIO (0, length proxies - 1)
    return $ Just (proxies !! index)

-- | Validate if a proxy is working
validateProxy :: ProxyConfig -> IO Bool
validateProxy proxy = do
    -- This is a simplified validation - in practice you'd want to test the proxy
    putStrLn $ "Validating proxy: " ++ proxyHost proxy ++ ":" ++ show (proxyPort proxy)
    return True -- Placeholder - implement actual validation

-- | Rotate to a new proxy
rotateProxy :: [ProxyConfig] -> IO (Maybe ProxyConfig)
rotateProxy proxies = do
    validProxies <- filterM validateProxy proxies
    getRandomProxy validProxies

-- | Create a WebDriver session with proxy configuration
createSessionWithProxy :: Maybe ProxyConfig -> Bool -> IO WDConfig
createSessionWithProxy mProxy headless = do
    let baseConfig = chromeConfig headless
    case mProxy of
        Just proxy -> do
            let proxyArg = "--proxy-server=" ++ proxyHost proxy ++ ":" ++ show (proxyPort proxy)
            let authArgs = case (proxyUsername proxy, proxyPassword proxy) of
                    (Just user, Just pass) -> ["--proxy-auth=" ++ user ++ ":" ++ pass]
                    _ -> []
            let proxyOpts = proxyArg : authArgs
            return $ useBrowser (chrome { chromeOptions = opts ++ proxyOpts }) defaultConfig
        Nothing -> return baseConfig
  where
    chromeConfig headless = useBrowser (chrome { chromeOptions = opts }) defaultConfig
      where
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