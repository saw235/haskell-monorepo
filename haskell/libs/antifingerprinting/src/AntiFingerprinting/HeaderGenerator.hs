{-# LANGUAGE OverloadedStrings #-}

module AntiFingerprinting.HeaderGenerator
  ( -- * Header Generation
    generateHeaders,
    generateHeadersWithOptions,
    generateUserAgentHeader,
    generateAcceptHeaders,
    generateSecHeaders,

    -- * Specific Header Generators
    generateAcceptLanguageHeader,
    generateAcceptEncodingHeader,
    generateCacheControlHeader,
    generateConnectionHeader,
    generateUpgradeInsecureRequestsHeader,

    -- * Header Options
    HeaderOptions (..),
    defaultHeaderOptions,
  )
where

import AntiFingerprinting.Types
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (StdGen, mkStdGen, randomR)

-- | Options for header generation
data HeaderOptions = HeaderOptions
  { headerOptionsUserAgent :: Maybe Text,
    headerOptionsAcceptLanguage :: [Locale],
    headerOptionsAcceptEncoding :: [Text],
    headerOptionsConnection :: Text,
    headerOptionsUpgradeInsecureRequests :: Bool,
    headerOptionsDNT :: Maybe Text,
    headerOptionsSecFetchDest :: Maybe Text,
    headerOptionsSecFetchMode :: Maybe Text,
    headerOptionsSecFetchSite :: Maybe Text,
    headerOptionsSecFetchUser :: Maybe Text
  }
  deriving (Show, Eq)

-- | Default header options
defaultHeaderOptions :: HeaderOptions
defaultHeaderOptions =
  HeaderOptions
    { headerOptionsUserAgent = Nothing,
      headerOptionsAcceptLanguage = ["en-US", "en"],
      headerOptionsAcceptEncoding = ["gzip", "deflate", "br"],
      headerOptionsConnection = "keep-alive",
      headerOptionsUpgradeInsecureRequests = True,
      headerOptionsDNT = Nothing,
      headerOptionsSecFetchDest = Just "document",
      headerOptionsSecFetchMode = Just "navigate",
      headerOptionsSecFetchSite = Just "none",
      headerOptionsSecFetchUser = Just "?1"
    }

-- | Generate a complete set of HTTP headers
generateHeaders :: UserAgent -> Navigator -> Map HeaderName HeaderValue
generateHeaders userAgent navigator =
  generateHeadersWithOptions userAgent navigator defaultHeaderOptions

-- | Generate HTTP headers with custom options
generateHeadersWithOptions :: UserAgent -> Navigator -> HeaderOptions -> Map HeaderName HeaderValue
generateHeadersWithOptions userAgent navigator options =
  Map.fromList $
    concat
      [ [generateUserAgentHeader userAgent options],
        generateAcceptHeaders (userAgentBrowser userAgent),
        [generateAcceptLanguageHeader navigator options],
        [generateAcceptEncodingHeader options],
        [generateCacheControlHeader],
        [generateConnectionHeader options],
        [generateUpgradeInsecureRequestsHeader options],
        generateSecHeaders (userAgentBrowser userAgent) options,
        generateOptionalHeaders navigator options
      ]

-- | Generate User-Agent header
generateUserAgentHeader :: UserAgent -> HeaderOptions -> (HeaderName, HeaderValue)
generateUserAgentHeader userAgent options =
  case headerOptionsUserAgent options of
    Just ua -> ("User-Agent", ua)
    Nothing -> ("User-Agent", userAgentString userAgent)

-- | Generate Accept headers based on browser
generateAcceptHeaders :: Browser -> [(HeaderName, HeaderValue)]
generateAcceptHeaders Chrome =
  [ ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7")
  ]
generateAcceptHeaders Firefox =
  [ ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8")
  ]
generateAcceptHeaders Safari =
  [ ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
  ]
generateAcceptHeaders Edge =
  [ ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7")
  ]

-- | Generate Accept-Language header
generateAcceptLanguageHeader :: Navigator -> HeaderOptions -> (HeaderName, HeaderValue)
generateAcceptLanguageHeader navigator options =
  ("Accept-Language", formatLanguages languages)
  where
    languages =
      if null (headerOptionsAcceptLanguage options)
        then [navigatorLanguage navigator]
        else headerOptionsAcceptLanguage options

    formatLanguages :: [Locale] -> Text
    formatLanguages [] = "en-US,en;q=0.9"
    formatLanguages [lang] = lang
    formatLanguages langs = T.intercalate "," $ zipWith formatLang langs [1.0, 0.9, 0.8, 0.7, 0.6, 0.5]

    formatLang :: Locale -> Double -> Text
    formatLang lang 1.0 = lang
    formatLang lang q = T.concat [lang, ";q=", T.pack (show q)]

-- | Generate Accept-Encoding header
generateAcceptEncodingHeader :: HeaderOptions -> (HeaderName, HeaderValue)
generateAcceptEncodingHeader options =
  ("Accept-Encoding", T.intercalate ", " (headerOptionsAcceptEncoding options))

-- | Generate Cache-Control header
generateCacheControlHeader :: (HeaderName, HeaderValue)
generateCacheControlHeader = ("Cache-Control", "max-age=0")

-- | Generate Connection header
generateConnectionHeader :: HeaderOptions -> (HeaderName, HeaderValue)
generateConnectionHeader options = ("Connection", headerOptionsConnection options)

-- | Generate Upgrade-Insecure-Requests header
generateUpgradeInsecureRequestsHeader :: HeaderOptions -> (HeaderName, HeaderValue)
generateUpgradeInsecureRequestsHeader options =
  if headerOptionsUpgradeInsecureRequests options
    then ("Upgrade-Insecure-Requests", "1")
    else ("Upgrade-Insecure-Requests", "0")

-- | Generate Sec-Fetch-* headers based on browser
generateSecHeaders :: Browser -> HeaderOptions -> [(HeaderName, HeaderValue)]
generateSecHeaders Chrome options = generateModernSecHeaders options
generateSecHeaders Edge options = generateModernSecHeaders options
generateSecHeaders Firefox options = [] -- Firefox doesn't send Sec-Fetch headers by default
generateSecHeaders Safari options = [] -- Safari doesn't send Sec-Fetch headers

generateModernSecHeaders :: HeaderOptions -> [(HeaderName, HeaderValue)]
generateModernSecHeaders options =
  catMaybes
    [ fmap (("Sec-Fetch-Dest",) . id) (headerOptionsSecFetchDest options),
      fmap (("Sec-Fetch-Mode",) . id) (headerOptionsSecFetchMode options),
      fmap (("Sec-Fetch-Site",) . id) (headerOptionsSecFetchSite options),
      fmap (("Sec-Fetch-User",) . id) (headerOptionsSecFetchUser options)
    ]
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes (Nothing : xs) = catMaybes xs
    catMaybes (Just x : xs) = x : catMaybes xs

-- | Generate optional headers
generateOptionalHeaders :: Navigator -> HeaderOptions -> [(HeaderName, HeaderValue)]
generateOptionalHeaders navigator options =
  catMaybes
    [ fmap (("DNT",) . id) (headerOptionsDNT options),
      fmap (("DNT",) . id) (navigatorDoNotTrack navigator)
    ]
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes (Nothing : xs) = catMaybes xs
    catMaybes (Just x : xs) = x : catMaybes xs

-- | Generate headers for specific request types
generateNavigationHeaders :: UserAgent -> Navigator -> Map HeaderName HeaderValue
generateNavigationHeaders userAgent navigator =
  generateHeadersWithOptions userAgent navigator navigationOptions
  where
    navigationOptions =
      defaultHeaderOptions
        { headerOptionsSecFetchDest = Just "document",
          headerOptionsSecFetchMode = Just "navigate",
          headerOptionsSecFetchSite = Just "none",
          headerOptionsSecFetchUser = Just "?1"
        }

-- | Generate headers for resource requests (images, CSS, JS)
generateResourceHeaders :: UserAgent -> Navigator -> Text -> Map HeaderName HeaderValue
generateResourceHeaders userAgent navigator referer =
  Map.insert "Referer" referer $
    generateHeadersWithOptions userAgent navigator resourceOptions
  where
    resourceOptions =
      defaultHeaderOptions
        { headerOptionsSecFetchDest = Just "empty",
          headerOptionsSecFetchMode = Just "cors",
          headerOptionsSecFetchSite = Just "same-origin",
          headerOptionsSecFetchUser = Nothing
        }

-- | Generate headers for AJAX requests
generateAjaxHeaders :: UserAgent -> Navigator -> Text -> Map HeaderName HeaderValue
generateAjaxHeaders userAgent navigator referer =
  Map.fromList [("X-Requested-With", "XMLHttpRequest")]
    `Map.union` Map.insert
      "Referer"
      referer
      (generateHeadersWithOptions userAgent navigator ajaxOptions)
  where
    ajaxOptions =
      defaultHeaderOptions
        { headerOptionsSecFetchDest = Just "empty",
          headerOptionsSecFetchMode = Just "cors",
          headerOptionsSecFetchSite = Just "same-origin",
          headerOptionsSecFetchUser = Nothing
        }
