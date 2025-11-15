{-# LANGUAGE OverloadedStrings #-}

module HackageClient
  ( queryPackageWithCache,
    module HackageClient.Types,
  )
where

import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import HackageClient.API (fetchPackageInfo)
import HackageClient.Cache (readCache, writeCache, versionedCacheKey)
import HackageClient.Parser (parsePackageResponse)
import HackageClient.Types
  ( CacheEntry (..),
    Package (..),
    QueryMetadata (..),
    QueryResult (..),
  )

-- | Query package with caching (T027, T047)
-- Checks cache first, fetches from Hackage on miss, writes to cache
-- Accepts optional version parameter for version-specific queries
queryPackageWithCache :: Text -> Maybe Text -> IO (Either String (QueryResult Package))
queryPackageWithCache pkgName maybeVersion = do
  startTime <- getCurrentTime
  let cacheKey = versionedCacheKey "pkg" pkgName maybeVersion

  -- Try to read from cache
  mCached <- readCache cacheKey :: IO (Maybe (CacheEntry Package))

  case mCached of
    Just cached -> do
      -- Cache hit
      endTime <- getCurrentTime
      let duration = realToFrac (diffUTCTime endTime startTime)
      let age = realToFrac (diffUTCTime endTime (cacheTimestamp cached))
      return $
        Right $
          QueryResult
            { queryData = cacheData cached,
              queryMetadata =
                QueryMetadata
                  { queryTime = endTime,
                    queryDuration = duration,
                    queryCacheHit = True,
                    queryCacheAge = Just age
                  },
              querySuggestions = []
            }
    Nothing -> do
      -- Cache miss - fetch from Hackage
      result <- fetchPackageInfo pkgName maybeVersion
      case result of
        Left err -> return $ Left err
        Right responseBody -> do
          case parsePackageResponse pkgName responseBody of
            Nothing -> return $ Left "Failed to parse package response"
            Just pkg -> do
              -- Write to cache with 24-hour TTL
              let ttl = secondsToNominalDiffTime 86400 -- 24 hours
              writeCache cacheKey pkg ttl
              endTime <- getCurrentTime
              let duration = realToFrac (diffUTCTime endTime startTime)
              return $
                Right $
                  QueryResult
                    { queryData = pkg,
                      queryMetadata =
                        QueryMetadata
                          { queryTime = endTime,
                            queryDuration = duration,
                            queryCacheHit = False,
                            queryCacheAge = Nothing
                          },
                      querySuggestions = []
                    }
