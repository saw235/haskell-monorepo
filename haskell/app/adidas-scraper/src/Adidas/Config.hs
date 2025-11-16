{-# LANGUAGE OverloadedStrings #-}

module Adidas.Config
  ( ScrapingConfig (..),
    defaultConfig,
    stealthConfig,
    aggressiveConfig,
    getConfigFromEnv,
  )
where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

-- | Configuration for scraping behavior
data ScrapingConfig = ScrapingConfig
  { useProxies :: Bool,
    useStealth :: Bool,
    maxRetries :: Int,
    delayBetweenRequests :: Int,
    sessionTimeout :: Int,
    userAgentRotation :: Bool,
    fingerprintRandomization :: Bool,
    humanizeBehavior :: Bool,
    respectRobotsTxt :: Bool,
    maxConcurrentSessions :: Int
  }
  deriving (Show, Eq)

-- | Default configuration (balanced approach)
defaultConfig :: ScrapingConfig
defaultConfig =
  ScrapingConfig
    { useProxies = False,
      useStealth = True,
      maxRetries = 3,
      delayBetweenRequests = 2000,
      sessionTimeout = 30000,
      userAgentRotation = True,
      fingerprintRandomization = True,
      humanizeBehavior = True,
      respectRobotsTxt = True,
      maxConcurrentSessions = 1
    }

-- | Stealth configuration (maximum stealth)
stealthConfig :: ScrapingConfig
stealthConfig =
  ScrapingConfig
    { useProxies = True,
      useStealth = True,
      maxRetries = 5,
      delayBetweenRequests = 5000,
      sessionTimeout = 60000,
      userAgentRotation = True,
      fingerprintRandomization = True,
      humanizeBehavior = True,
      respectRobotsTxt = True,
      maxConcurrentSessions = 1
    }

-- | Aggressive configuration (faster but more detectable)
aggressiveConfig :: ScrapingConfig
aggressiveConfig =
  ScrapingConfig
    { useProxies = False,
      useStealth = False,
      maxRetries = 1,
      delayBetweenRequests = 500,
      sessionTimeout = 15000,
      userAgentRotation = False,
      fingerprintRandomization = False,
      humanizeBehavior = False,
      respectRobotsTxt = False,
      maxConcurrentSessions = 3
    }

-- | Get configuration from environment variables
getConfigFromEnv :: IO ScrapingConfig
getConfigFromEnv = do
  useProxiesEnv <- lookupEnv "ADIDAS_USE_PROXIES"
  useStealthEnv <- lookupEnv "ADIDAS_USE_STEALTH"
  maxRetriesEnv <- lookupEnv "ADIDAS_MAX_RETRIES"
  delayEnv <- lookupEnv "ADIDAS_DELAY"
  timeoutEnv <- lookupEnv "ADIDAS_TIMEOUT"

  return $
    ScrapingConfig
      { useProxies = maybe False (== "true") useProxiesEnv,
        useStealth = maybe True (== "true") useStealthEnv,
        maxRetries = maybe 3 read maxRetriesEnv,
        delayBetweenRequests = maybe 2000 read delayEnv,
        sessionTimeout = maybe 30000 read timeoutEnv,
        userAgentRotation = True,
        fingerprintRandomization = True,
        humanizeBehavior = True,
        respectRobotsTxt = True,
        maxConcurrentSessions = 1
      }
