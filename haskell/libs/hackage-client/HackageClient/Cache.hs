{-# LANGUAGE OverloadedStrings #-}

module HackageClient.Cache
  ( cacheDir,
    readCache,
    writeCache,
    cacheFilePath,
    versionedCacheKey,
    moduleCacheKey,
  )
where

import Control.Exception (catch, IOException)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import HackageClient.Types (CacheEntry (..))
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

-- | Get the cache directory path, creating it if it doesn't exist
-- Location: ~/.cache/hackage-cli/
cacheDir :: IO FilePath
cacheDir = do
  base <- getXdgDirectory XdgCache "hackage-cli"
  createDirectoryIfMissing True base
  return base

-- | Generate cache file path from cache key
-- Cache key format examples:
--   - pkg:{packageName}
--   - versions:{packageName}
--   - tree:{packageName}-{version}
--   - module:{packageName}-{version}-{moduleName}
cacheFilePath :: Text -> IO FilePath
cacheFilePath key = do
  dir <- cacheDir
  let fileName = T.unpack (T.replace ":" "-" (T.replace "/" "-" key)) <> ".json"
  return $ dir </> fileName

-- | Write data to cache with TTL
writeCache ::
  (ToJSON a) =>
  Text -> -- cache key
  a -> -- data to cache
  NominalDiffTime -> -- TTL in seconds
  IO ()
writeCache key dat ttl = do
  now <- getCurrentTime
  path <- cacheFilePath key
  let entry =
        CacheEntry
          { cacheData = dat,
            cacheTimestamp = now,
            cacheTTL = ttl,
            cacheKey = key
          }
  encodeFile path entry

-- | Read data from cache, checking TTL validity
-- Returns Nothing if cache doesn't exist or is expired
readCache :: (FromJSON a) => Text -> IO (Maybe (CacheEntry a))
readCache key = do
  path <- cacheFilePath key
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      mEntry <- (decodeFileStrict path `catch` \(_ :: IOException) -> return Nothing)
      case mEntry of
        Nothing -> return Nothing
        Just entry -> do
          now <- getCurrentTime
          if diffUTCTime now (cacheTimestamp entry) < cacheTTL entry
            then return (Just entry)
            else return Nothing

-- | Generate version-specific cache key (T049)
-- Cache key format:
--   - With version: "{prefix}:{package}-{version}"
--   - Without version: "{prefix}:{package}"
-- Examples:
--   - versionedCacheKey "tree" "aeson" (Just "2.2.3.0") -> "tree:aeson-2.2.3.0"
--   - versionedCacheKey "tree" "aeson" Nothing -> "tree:aeson"
versionedCacheKey ::
  Text -> -- prefix (e.g., "tree", "module", "pkg")
  Text -> -- package name
  Maybe Text -> -- optional version
  Text
versionedCacheKey prefix pkgName maybeVersion =
  case maybeVersion of
    Just version -> prefix <> ":" <> pkgName <> "-" <> version
    Nothing -> prefix <> ":" <> pkgName

-- | Generate module-specific cache key (T065)
-- Cache key format: "module:{package}-{version}-{moduleName}"
-- Example: moduleCacheKey "aeson" "2.2.3.0" "Data.Aeson" -> "module:aeson-2.2.3.0-Data.Aeson"
moduleCacheKey ::
  Text -> -- package name
  Text -> -- version
  Text -> -- module name
  Text
moduleCacheKey pkgName version moduleName =
  "module:" <> pkgName <> "-" <> version <> "-" <> moduleName
