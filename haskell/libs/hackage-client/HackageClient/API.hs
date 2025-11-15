{-# LANGUAGE OverloadedStrings #-}

module HackageClient.API
  ( hackageRequest,
    hackageBaseUrl,
    fetchPackageInfo,
    fetchModuleDetails,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import HackageClient.Types (Package)
import Network.HTTP.Simple
  ( Request,
    Response,
    addRequestHeader,
    getResponseBody,
    getResponseStatusCode,
    httpBS,
    parseRequest,
  )

-- | Base URL for Hackage API
hackageBaseUrl :: String
hackageBaseUrl = "https://hackage.haskell.org"

-- | Make a GET request to Hackage with proper headers
-- Returns the response body as ByteString
hackageRequest :: String -> IO (Response BS.ByteString)
hackageRequest path = do
  let url = hackageBaseUrl ++ path
  request <- parseRequest url
  let requestWithHeaders =
        addRequestHeader "User-Agent" "hackage-doc-cli/0.1.0 (Haskell)" $
          addRequestHeader "Accept" "application/json, text/html" request
  httpBS requestWithHeaders

-- | Fetch package information from Hackage API (T025, T047)
-- Accepts optional version parameter for version-specific queries
-- Returns the raw response for parsing
fetchPackageInfo :: Text -> Maybe Text -> IO (Either String BL.ByteString)
fetchPackageInfo pkgName maybeVersion = do
  let path = case maybeVersion of
        Just version -> "/package/" ++ T.unpack pkgName ++ "-" ++ T.unpack version
        Nothing -> "/package/" ++ T.unpack pkgName
  response <- hackageRequest path
  let statusCode = getResponseStatusCode response
  if statusCode == 200
    then return $ Right $ BL.fromStrict $ getResponseBody response
    else return $ Left $ "HTTP " ++ show statusCode ++ " error for package: " ++ T.unpack pkgName

-- | Fetch module documentation HTML from Haddock (T056)
-- Retrieves the Haddock-generated HTML for a specific module
fetchModuleDetails :: Text -> Text -> Text -> IO (Either String BS.ByteString)
fetchModuleDetails pkgName version moduleName = do
  -- Convert module name like "Data.Aeson" to "Data-Aeson.html"
  let moduleFileName = T.replace "." "-" moduleName <> ".html"
  let path =
        "/package/"
          ++ T.unpack pkgName
          ++ "-"
          ++ T.unpack version
          ++ "/docs/"
          ++ T.unpack moduleFileName
  response <- hackageRequest path
  let statusCode = getResponseStatusCode response
  if statusCode == 200
    then return $ Right $ getResponseBody response
    else
      return $
        Left $
          "HTTP "
            ++ show statusCode
            ++ " error for module: "
            ++ T.unpack moduleName
            ++ " in package "
            ++ T.unpack pkgName
