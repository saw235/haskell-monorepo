{-# LANGUAGE OverloadedStrings #-}

module HeaderOrderCollector.Collector
  ( -- * Header Order Detection
    detectHeaderOrder
  , matchHeaderPattern
  , identifyBrowser
  
    -- * Pattern Operations
  , createPattern
  , updatePattern
  , scorePatternMatch
  
    -- * Database Operations
  , createDatabase
  , updateDatabase
  , queryDatabase
  , addBrowserProfile
  
    -- * Order Generation
  , generateRealisticOrder
  , orderHeadersForBrowser
  , applyOrderingRule
  
    -- * Statistics
  , analyzeHeaderFrequency
  , calculatePatternDistribution
  , updateStatistics
  
    -- * Default Patterns
  , defaultChromeOrder
  , defaultFirefoxOrder
  , defaultSafariOrder
  , defaultEdgeOrder
  ) where

import HeaderOrderCollector.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, nub, intersect, (\\))
import Data.Ord (comparing)
import Data.Maybe (catMaybes, fromMaybe)

-- | Detect the header order pattern from a list of headers
detectHeaderOrder :: [HeaderName] -> BrowserType -> DeviceType -> OperatingSystem -> HeaderOrder
detectHeaderOrder headers browser device os =
  HeaderOrder
    { orderHeaders = headers
    , orderBrowser = browser
    , orderVersion = BrowserVersion 0 0 0 Nothing  -- Unknown version
    , orderDevice = device
    , orderOS = os
    , orderContext = "unknown"
    , orderFrequency = 1.0
    }

-- | Match headers against known patterns
matchHeaderPattern :: [HeaderName] -> HeaderDatabase -> [(HeaderPattern, Double)]
matchHeaderPattern headers database =
  let patterns = Map.elems (databasePatterns database)
      matches = [(pattern, scorePatternMatch headers pattern) | pattern <- patterns]
      validMatches = filter (\(_, score) -> score > 0.0) matches
  in sortBy (comparing (negate . snd)) validMatches

-- | Identify browser based on header order patterns
identifyBrowser :: [HeaderName] -> HeaderDatabase -> Maybe (BrowserType, Double)
identifyBrowser headers database =
  case matchHeaderPattern headers database of
    [] -> Nothing
    ((pattern, score):_) -> 
      let browserGuess = guessBrowserFromPattern pattern
      in Just (browserGuess, score)

-- | Create a new header pattern
createPattern :: Text -> Text -> [HeaderName] -> [PatternRule] -> HeaderPattern
createPattern patternId name headers rules =
  HeaderPattern
    { patternId = patternId
    , patternName = name
    , patternHeaders = headers
    , patternRules = rules
    , patternScore = 1.0
    , patternMetadata = Map.empty
    }

-- | Update an existing pattern with new data
updatePattern :: HeaderPattern -> [HeaderName] -> HeaderPattern
updatePattern pattern newHeaders =
  pattern { patternHeaders = mergeHeaderLists (patternHeaders pattern) newHeaders }

-- | Score how well headers match a pattern
scorePatternMatch :: [HeaderName] -> HeaderPattern -> Double
scorePatternMatch headers pattern =
  let patHeaders = patternHeaders pattern
      rules = patternRules pattern
      -- Basic order similarity
      orderScore = calculateOrderSimilarity headers patHeaders
      -- Rule compliance score
      ruleScore = calculateRuleCompliance headers rules
      -- Combined score
  in (orderScore + ruleScore) / 2.0

-- | Calculate similarity between two header orders
calculateOrderSimilarity :: [HeaderName] -> [HeaderName] -> Double
calculateOrderSimilarity actual expected =
  let commonHeaders = intersect actual expected
      totalHeaders = fromIntegral $ length expected
      commonCount = fromIntegral $ length commonHeaders
      
      -- Check if common headers are in the same relative order
      orderMatches = countOrderMatches actual expected commonHeaders
      maxOrderMatches = fromIntegral $ length commonHeaders
      
      presence = if totalHeaders > 0 then commonCount / totalHeaders else 0.0
      ordering = if maxOrderMatches > 0 then orderMatches / maxOrderMatches else 1.0
      
  in (presence + ordering) / 2.0

-- | Count how many headers are in the correct relative order
countOrderMatches :: [HeaderName] -> [HeaderName] -> [HeaderName] -> Double
countOrderMatches actual expected common =
  let actualPositions = Map.fromList $ zip actual [0..]
      expectedPositions = Map.fromList $ zip expected [0..]
      
      getPosition header map = fromMaybe (-1) (Map.lookup header map)
      
      -- Count pairs that maintain relative order
      pairs = [(h1, h2) | h1 <- common, h2 <- common, h1 /= h2]
      correctPairs = filter (\(h1, h2) -> 
        let aPos1 = getPosition h1 actualPositions
            aPos2 = getPosition h2 actualPositions
            ePos1 = getPosition h1 expectedPositions
            ePos2 = getPosition h2 expectedPositions
        in (aPos1 < aPos2) == (ePos1 < ePos2)
        ) pairs
        
  in fromIntegral (length correctPairs)

-- | Calculate rule compliance score
calculateRuleCompliance :: [HeaderName] -> [PatternRule] -> Double
calculateRuleCompliance headers rules =
  let ruleResults = map (evaluateRule headers) rules
      passedRules = length $ filter id ruleResults
      totalRules = length rules
  in if totalRules > 0 
     then fromIntegral passedRules / fromIntegral totalRules 
     else 1.0

-- | Evaluate a single pattern rule
evaluateRule :: [HeaderName] -> PatternRule -> Bool
evaluateRule headers rule =
  case rule of
    MustHaveHeader header -> header `elem` headers
    MustNotHaveHeader header -> header `notElem` headers
    HeaderMustBeAfter h1 h2 -> isHeaderAfter h1 h2 headers
    HeaderMustBeBefore h1 h2 -> isHeaderBefore h1 h2 headers
    HeaderMustBeAtPosition header pos -> getHeaderPosition header headers == Just pos
    ConsecutiveHeaders headerList -> areHeadersConsecutive headerList headers
    ConditionalRule _ subRule -> evaluateRule headers subRule  -- Simplified

-- | Check if header h1 comes after h2
isHeaderAfter :: HeaderName -> HeaderName -> [HeaderName] -> Bool
isHeaderAfter h1 h2 headers =
  case (getHeaderPosition h1 headers, getHeaderPosition h2 headers) of
    (Just pos1, Just pos2) -> pos1 > pos2
    _ -> False

-- | Check if header h1 comes before h2
isHeaderBefore :: HeaderName -> HeaderName -> [HeaderName] -> Bool
isHeaderBefore h1 h2 headers = isHeaderAfter h2 h1 headers

-- | Get position of header in list
getHeaderPosition :: HeaderName -> [HeaderName] -> Maybe Int
getHeaderPosition header headers = 
  case lookup header (zip headers [0..]) of
    Just pos -> Just pos
    Nothing -> Nothing

-- | Check if headers appear consecutively
areHeadersConsecutive :: [HeaderName] -> [HeaderName] -> Bool
areHeadersConsecutive targetHeaders allHeaders =
  any (isSubsequence targetHeaders) (windows (length targetHeaders) allHeaders)
  where
    windows n xs = [take n (drop i xs) | i <- [0..length xs - n]]
    isSubsequence [] _ = True
    isSubsequence _ [] = False
    isSubsequence (x:xs) (y:ys)
      | x == y = isSubsequence xs ys
      | otherwise = False

-- | Create empty database
createDatabase :: HeaderDatabase
createDatabase = HeaderDatabase
  { databaseProfiles = Map.empty
  , databasePatterns = Map.empty
  , databaseStatistics = createEmptyStatistics
  , databaseVersion = "1.0.0"
  }

-- | Create empty statistics
createEmptyStatistics :: HeaderStatistics
createEmptyStatistics = HeaderStatistics
  { statsHeaderFrequency = Map.empty
  , statsOrderFrequency = Map.empty
  , statsBrowserDistribution = Map.empty
  , statsDeviceDistribution = Map.empty
  , statsCommonPatterns = []
  , statsTotalSamples = 0
  }

-- | Update database with new header order
updateDatabase :: HeaderDatabase -> HeaderOrder -> HeaderDatabase
updateDatabase database order =
  let updatedStats = updateStatistics (databaseStatistics database) order
      updatedProfiles = updateBrowserProfile (databaseProfiles database) order
  in database 
     { databaseStatistics = updatedStats
     , databaseProfiles = updatedProfiles
     }

-- | Query database for matching patterns
queryDatabase :: HeaderDatabase -> [HeaderName] -> BrowserType -> [HeaderOrder]
queryDatabase database headers browser =
  case Map.lookup browser (databaseProfiles database) of
    Nothing -> []
    Just profile -> filter (matchesHeaders headers) (profileHeaderOrders profile)
  where
    matchesHeaders :: [HeaderName] -> HeaderOrder -> Bool
    matchesHeaders queryHeaders order =
      let commonHeaders = intersect queryHeaders (orderHeaders order)
          similarity = fromIntegral (length commonHeaders) / fromIntegral (length queryHeaders)
      in similarity > 0.7

-- | Add browser profile to database
addBrowserProfile :: HeaderDatabase -> BrowserProfile -> HeaderDatabase
addBrowserProfile database profile =
  database { databaseProfiles = Map.insert (profileBrowser profile) profile (databaseProfiles database) }

-- | Generate realistic header order for a browser
generateRealisticOrder :: HeaderDatabase -> BrowserType -> DeviceType -> OperatingSystem -> [HeaderName] -> [HeaderName]
generateRealisticOrder database browser device os baseHeaders =
  case Map.lookup browser (databaseProfiles database) of
    Nothing -> orderHeadersForBrowser browser baseHeaders  -- Fallback to default ordering
    Just profile -> 
      let matchingOrders = filter (\order -> 
            orderDevice order == device && 
            orderOS order == os) (profileHeaderOrders profile)
      in case matchingOrders of
           [] -> orderHeadersForBrowser browser baseHeaders
           (bestOrder:_) -> mergeWithTemplate (orderHeaders bestOrder) baseHeaders

-- | Order headers according to browser-specific patterns
orderHeadersForBrowser :: BrowserType -> [HeaderName] -> [HeaderName]
orderHeadersForBrowser browser headers =
  let template = getBrowserTemplate browser
      known = intersect template headers
      unknown = headers \\ template
  in known ++ unknown

-- | Get default header template for browser
getBrowserTemplate :: BrowserType -> [HeaderName]
getBrowserTemplate Chrome = defaultChromeOrder
getBrowserTemplate Firefox = defaultFirefoxOrder
getBrowserTemplate Safari = defaultSafariOrder
getBrowserTemplate Edge = defaultEdgeOrder
getBrowserTemplate _ = defaultChromeOrder  -- Fallback

-- | Apply ordering rule to headers
applyOrderingRule :: OrderingRule -> [HeaderName] -> [HeaderName]
applyOrderingRule rule headers =
  case rule of
    AlphabeticalOrder -> sortBy compare headers
    FrequencyOrder -> sortBy (comparing (negate . getHeaderImportance)) headers
    BrowserSpecificOrder browser -> orderHeadersForBrowser browser headers
    CustomOrder template -> mergeWithTemplate template headers
    PriorityOrder priorities -> sortBy (comparing (getPriority priorities)) headers
  where
    getHeaderImportance :: HeaderName -> Int
    getHeaderImportance "User-Agent" = 10
    getHeaderImportance "Accept" = 9
    getHeaderImportance "Accept-Language" = 8
    getHeaderImportance "Accept-Encoding" = 7
    getHeaderImportance _ = 1
    
    getPriority :: Map HeaderName Int -> HeaderName -> Int
    getPriority priorities header = fromMaybe 0 (Map.lookup header priorities)

-- | Merge headers with a template order
mergeWithTemplate :: [HeaderName] -> [HeaderName] -> [HeaderName]
mergeWithTemplate template headers =
  let templateHeaders = filter (`elem` headers) template
      extraHeaders = headers \\ template
  in templateHeaders ++ extraHeaders

-- | Merge two header lists intelligently
mergeHeaderLists :: [HeaderName] -> [HeaderName] -> [HeaderName]
mergeHeaderLists list1 list2 = nub (list1 ++ list2)

-- | Analyze header frequency in database
analyzeHeaderFrequency :: HeaderDatabase -> Map HeaderName HeaderFrequency
analyzeHeaderFrequency database = statsHeaderFrequency (databaseStatistics database)

-- | Calculate pattern distribution
calculatePatternDistribution :: HeaderDatabase -> [PatternDistribution]
calculatePatternDistribution database = statsCommonPatterns (databaseStatistics database)

-- | Update statistics with new header order
updateStatistics :: HeaderStatistics -> HeaderOrder -> HeaderStatistics
updateStatistics stats order =
  let headers = orderHeaders order
      browser = orderBrowser order
      device = orderDevice order
      
      -- Update header frequency
      updatedHeaderFreq = foldl (\acc header -> 
        Map.insertWith (+) header 1.0 acc) (statsHeaderFrequency stats) headers
      
      -- Update browser distribution
      updatedBrowserDist = Map.insertWith (+) browser 1.0 (statsBrowserDistribution stats)
      
      -- Update device distribution  
      updatedDeviceDist = Map.insertWith (+) device 1.0 (statsDeviceDistribution stats)
      
  in stats
     { statsHeaderFrequency = updatedHeaderFreq
     , statsBrowserDistribution = updatedBrowserDist
     , statsDeviceDistribution = updatedDeviceDist
     , statsTotalSamples = statsTotalSamples stats + 1
     }

-- | Update browser profile with new order
updateBrowserProfile :: Map BrowserType BrowserProfile -> HeaderOrder -> Map BrowserType BrowserProfile
updateBrowserProfile profiles order =
  let browser = orderBrowser order
      defaultProfile = createDefaultProfile browser
  in Map.insertWith mergeProfiles browser defaultProfile profiles
  where
    mergeProfiles newProfile oldProfile = 
      oldProfile { profileHeaderOrders = order : profileHeaderOrders oldProfile }

-- | Create default profile for browser
createDefaultProfile :: BrowserType -> BrowserProfile
createDefaultProfile browser =
  BrowserProfile
    { profileBrowser = browser
    , profileVersionRange = (BrowserVersion 0 0 0 Nothing, BrowserVersion 999 0 0 Nothing)
    , profileDevices = [Desktop, Mobile, Tablet]
    , profileOperatingSystems = [Windows, MacOS, Linux, Android, IOS]
    , profileHeaderOrders = []
    , profilePatterns = []
    , profileLastUpdated = "2024-01-01"
    }

-- | Guess browser from pattern metadata
guessBrowserFromPattern :: HeaderPattern -> BrowserType
guessBrowserFromPattern pattern =
  case Map.lookup "browser" (patternMetadata pattern) of
    Just "chrome" -> Chrome
    Just "firefox" -> Firefox
    Just "safari" -> Safari
    Just "edge" -> Edge
    _ -> Chrome  -- Default fallback

-- | Default header orders for different browsers
defaultChromeOrder :: [HeaderName]
defaultChromeOrder = 
  [ "User-Agent"
  , "Accept"
  , "Accept-Language"
  , "Accept-Encoding"
  , "Cache-Control"
  , "Connection"
  , "Upgrade-Insecure-Requests"
  , "Sec-Fetch-Dest"
  , "Sec-Fetch-Mode"
  , "Sec-Fetch-Site"
  , "Sec-Fetch-User"
  ]

defaultFirefoxOrder :: [HeaderName]
defaultFirefoxOrder = 
  [ "User-Agent"
  , "Accept"
  , "Accept-Language"
  , "Accept-Encoding"
  , "Connection"
  , "Upgrade-Insecure-Requests"
  , "TE"
  ]

defaultSafariOrder :: [HeaderName]
defaultSafariOrder = 
  [ "User-Agent"
  , "Accept"
  , "Accept-Language"
  , "Accept-Encoding"
  , "Connection"
  , "Upgrade-Insecure-Requests"
  ]

defaultEdgeOrder :: [HeaderName]
defaultEdgeOrder = 
  [ "User-Agent"
  , "Accept"
  , "Accept-Language"
  , "Accept-Encoding"
  , "Cache-Control"
  , "Connection"
  , "Upgrade-Insecure-Requests"
  , "Sec-Fetch-Dest"
  , "Sec-Fetch-Mode"
  , "Sec-Fetch-Site"
  , "Sec-Fetch-User"
  ] 