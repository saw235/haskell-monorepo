{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AgenticFramework.Workflow.Cache
-- Description : Capability caching with TTL
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Provides caching for loaded capabilities with configurable TTL.
-- Default TTL is 24 hours for capability definitions.
module AgenticFramework.Workflow.Cache
  ( -- * Cache Types
    CapabilityCache,
    CacheConfig (..),
    CachedCapability (..),

    -- * Cache Operations
    newCache,
    getCached,
    putCached,
    invalidate,
    invalidateAll,
    isExpired,

    -- * Default Configuration
    defaultCacheConfig,
    defaultTTL,
  )
where

import AgenticFramework.Workflow.Types (Capability)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime)

-- | Default TTL for cached capabilities (24 hours in seconds)
defaultTTL :: Integer
defaultTTL = 24 * 60 * 60

-- | Cache configuration
data CacheConfig = CacheConfig
  { cacheTTLSeconds :: Integer, -- Time to live in seconds
    cacheMaxSize :: Maybe Int -- Maximum number of cached items (Nothing = unlimited)
  }
  deriving (Show, Eq)

-- | Default cache configuration
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { cacheTTLSeconds = defaultTTL,
      cacheMaxSize = Nothing
    }

-- | A cached capability with metadata
data CachedCapability = CachedCapability
  { cachedCapability :: Capability,
    cachedAt :: UTCTime,
    cachedSource :: Text -- File path or identifier
  }
  deriving (Show)

-- | The capability cache
data CapabilityCache = CapabilityCache
  { cacheStore :: MVar (Map Text CachedCapability),
    cacheConfig :: CacheConfig
  }

-- | Create a new empty cache
newCache :: CacheConfig -> IO CapabilityCache
newCache config = do
  store <- newMVar Map.empty
  return $
    CapabilityCache
      { cacheStore = store,
        cacheConfig = config
      }

-- | Get a capability from the cache if it exists and is not expired
getCached :: CapabilityCache -> Text -> IO (Maybe Capability)
getCached cache key = do
  now <- getCurrentTime
  store <- readMVar (cacheStore cache)
  case Map.lookup key store of
    Nothing -> return Nothing
    Just cached ->
      if isExpired (cacheConfig cache) now cached
        then do
          -- Remove expired entry
          modifyMVar_ (cacheStore cache) $ \s -> return $ Map.delete key s
          return Nothing
        else return $ Just (cachedCapability cached)

-- | Put a capability in the cache
putCached :: CapabilityCache -> Text -> Capability -> Text -> IO ()
putCached cache key cap source = do
  now <- getCurrentTime
  let entry =
        CachedCapability
          { cachedCapability = cap,
            cachedAt = now,
            cachedSource = source
          }
  modifyMVar_ (cacheStore cache) $ \store -> do
    let newStore = Map.insert key entry store
    -- Enforce max size if configured
    case cacheMaxSize (cacheConfig cache) of
      Nothing -> return newStore
      Just maxSize ->
        if Map.size newStore > maxSize
          then return $ removeOldest newStore
          else return newStore

-- | Remove the oldest entry from the cache
removeOldest :: Map Text CachedCapability -> Map Text CachedCapability
removeOldest store
  | Map.null store = store
  | otherwise =
      let oldest = Map.foldrWithKey findOldest Nothing store
       in case oldest of
            Nothing -> store
            Just key -> Map.delete key store
  where
    findOldest k v Nothing = Just k
    findOldest k v (Just oldK) =
      case Map.lookup oldK store of
        Nothing -> Just k
        Just oldV ->
          if cachedAt v < cachedAt oldV
            then Just k
            else Just oldK

-- | Invalidate a specific cached capability
invalidate :: CapabilityCache -> Text -> IO ()
invalidate cache key =
  modifyMVar_ (cacheStore cache) $ \store ->
    return $ Map.delete key store

-- | Invalidate all cached capabilities
invalidateAll :: CapabilityCache -> IO ()
invalidateAll cache =
  modifyMVar_ (cacheStore cache) $ \_ ->
    return Map.empty

-- | Check if a cached capability is expired
isExpired :: CacheConfig -> UTCTime -> CachedCapability -> Bool
isExpired config now cached =
  let ttl = fromIntegral (cacheTTLSeconds config)
      expiry = addUTCTime ttl (cachedAt cached)
   in now > expiry
