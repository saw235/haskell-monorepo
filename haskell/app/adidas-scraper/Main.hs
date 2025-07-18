{-# LANGUAGE OverloadedStrings #-}

module Main where

import Adidas.AntiFingerprint (humanizeScrolling, mockComprehensiveFingerprint, randomizeUserAgent, setupAntiFingerprinting, setupAntiFingerprintingWithProfile)
import Adidas.FingerprintConfig (FingerprintProfile (..), chromeProfile, defaultProfile, firefoxProfile, safariProfile)
import Adidas.NavScraper (scrapeNavLinks)
import Adidas.Scraper (scrapeProducts)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import System.Environment (getArgs)
import System.Random (randomRIO)
import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.Commands (closeSession, closeWindow, getCurrentWindow, windows)
import Test.WebDriver.Monad (WD)
import Text.HTML.Scalpel.Core (scrapeStringLike)

-- | Configuration for Chrome WebDriver with anti-fingerprinting
chromeConfig :: Bool -> WDConfig
chromeConfig headless = useBrowser (chrome {chromeOptions = opts}) defaultConfig
  where
    -- Enhanced anti-fingerprinting options
    baseOpts =
      [ "--no-sandbox",
        "--disable-dev-shm-usage",
        "--disable-blink-features=AutomationControlled",
        "--disable-extensions",
        "--disable-plugins-discovery",
        "--disable-default-apps",
        "--disable-sync",
        "--disable-translate",
        "--disable-background-timer-throttling",
        "--disable-backgrounding-occluded-windows",
        "--disable-renderer-backgrounding",
        "--disable-features=TranslateUI",
        "--disable-ipc-flooding-protection",
        "--no-first-run",
        "--no-default-browser-check",
        "--disable-web-security",
        "--disable-features=VizDisplayCompositor"
      ]
    headlessOpts = if headless then ["--headless"] else ["--start-minimized"]
    opts = headlessOpts ++ baseOpts

-- | CLI Options
data PageOptions = PageOptions
  { pageUrl :: String,
    pageOutputFile :: FilePath,
    pageDelay :: Int,
    pageProfile :: String
  }

data NavLinkOptions = NavLinkOptions
  { navOutputFile :: Maybe FilePath,
    navAll :: Bool
  }

data Command
  = ScrapePage PageOptions
  | ScrapeNavLinks NavLinkOptions

parsePageOptions :: OA.Parser PageOptions
parsePageOptions =
  PageOptions
    <$> OA.strOption
      ( OA.long "url"
          <> OA.metavar "URL"
          <> OA.help "The Adidas product page URL to scrape"
      )
    <*> OA.strOption
      ( OA.long "output"
          <> OA.metavar "FILE"
          <> OA.help "The output file for the JSON data"
          <> OA.value "products.json"
          <> OA.showDefault
      )
    <*> OA.option
      OA.auto
      ( OA.long "delay"
          <> OA.metavar "SECONDS"
          <> OA.help "Delay between scrolls in seconds (default: 3)"
          <> OA.value 3
          <> OA.showDefault
      )
    <*> OA.strOption
      ( OA.long "profile"
          <> OA.metavar "PROFILE"
          <> OA.help "Fingerprint profile: default, firefox, chrome, safari (default: default)"
          <> OA.value "default"
          <> OA.showDefault
      )

parseNavLinkOptions :: OA.Parser NavLinkOptions
parseNavLinkOptions =
  NavLinkOptions
    <$> OA.optional
      ( OA.strOption
          ( OA.long "output"
              <> OA.metavar "FILE"
              <> OA.help "The output file for the navigation links JSON"
          )
      )
    <*> OA.switch
      ( OA.long "all"
          <> OA.help "Include all links (not just category links)"
      )

parseCommand :: OA.Parser Command
parseCommand =
  OA.hsubparser
    ( OA.command
        "scrape-page"
        ( OA.info
            (ScrapePage <$> parsePageOptions)
            (OA.progDesc "Scrape Adidas products from a given URL and output as JSON")
        )
        <> OA.command
          "scrape-nav-links"
          ( OA.info
              (ScrapeNavLinks <$> parseNavLinkOptions)
              (OA.progDesc "Scrape all category URLs from the Adidas navigation menu and output as JSON")
          )
    )

optsParserInfo :: OA.ParserInfo Command
optsParserInfo =
  OA.info
    (parseCommand OA.<**> OA.helper)
    (OA.fullDesc <> OA.progDesc "Adidas scraper CLI")

-- | Get fingerprint profile based on CLI option
getFingerprintProfile :: String -> FingerprintProfile
getFingerprintProfile "firefox" = firefoxProfile
getFingerprintProfile "chrome" = chromeProfile
getFingerprintProfile "safari" = safariProfile
getFingerprintProfile _ = defaultProfile

-- | Enhanced anti-fingerprinting setup with profile selection
setupAntiFingerprintingEnhanced :: String -> WD ()
setupAntiFingerprintingEnhanced profile = do
  -- Get the fingerprint profile based on the string
  let fingerprintProfile = getFingerprintProfile profile

  -- Apply comprehensive anti-fingerprinting with the specific profile
  setupAntiFingerprintingWithProfile fingerprintProfile

  -- Additional stealth measures
  _ <- (executeJS [] "delete window.cdc_adoQpoasnfa76pfcZLmcfl_Array; delete window.cdc_adoQpoasnfa76pfcZLmcfl_Promise; delete window.cdc_adoQpoasnfa76pfcZLmcfl_Symbol; window.chrome = {runtime: {}, loadTimes: function() {}, csi: function() {}, app: {}};" :: WD ())
  pure ()

-- | Count the number of product cards on the page
countProductCards :: WD Int
countProductCards = do
  elems <- findElems (ByCSS ".product-card")
  return (length elems)

-- | Humanized scrolling to the bottom of the page
scrollToBottom :: WD ()
scrollToBottom = do
  humanizeScrolling
  pure ()

-- | Perform infinite scroll until no new products are loaded
infiniteScroll :: String -> IO () -> Int -> String -> WD T.Text
infiniteScroll url logAction delaySec profile = do
  -- Navigate to a blank page first
  openPage "about:blank"

  -- Apply comprehensive anti-fingerprinting with profile BEFORE navigating to target site
  setupAntiFingerprintingEnhanced profile

  -- Now navigate to the target URL with patches already in place
  openPage url

  -- Add initial random delay to simulate human behavior
  initialDelay <- liftIO $ randomRIO (1000 :: Int, 3000 :: Int)
  liftIO $ threadDelay (initialDelay * 1000)

  let scrollLoop prevCount = do
        scrollToBottom
        -- Random delay between scrolls
        randomDelay <- liftIO $ randomRIO (delaySec * 500 :: Int, delaySec * 1500 :: Int)
        liftIO $ threadDelay (randomDelay * 1000)
        currCount <- countProductCards
        liftIO $ logAction >> putStrLn ("Scrolled. Product count: " ++ show currCount)
        if currCount == prevCount || currCount == 0
          then getSource
          else scrollLoop currCount
  initialCount <- countProductCards
  liftIO $ logAction >> putStrLn ("Initial product count: " ++ show initialCount)
  scrollLoop initialCount

-- | Output the scraped products in the desired format
outputProducts :: (ToJSON a) => Bool -> Maybe FilePath -> [a] -> (a -> String) -> IO ()
outputProducts jsonOut mOutputPath prods showFn =
  let jsonBytes = encodePretty prods
   in case mOutputPath of
        Just path -> do
          if jsonOut
            then BL.writeFile path jsonBytes
            else writeFile path (unlines (map showFn prods))
          putStrLn $ "Output written to " ++ path
        Nothing -> do
          if jsonOut
            then BL.putStr jsonBytes
            else mapM_ (putStrLn . showFn) prods

main :: IO ()
main = do
  cmd <- OA.execParser optsParserInfo
  case cmd of
    ScrapePage opts -> do
      let url = pageUrl opts
          outputFile = pageOutputFile opts
          delaySec = pageDelay opts
          profile = pageProfile opts
          headless = False -- Default to non-headless mode; can be extended if needed
      putStrLn $ "Starting Adidas scraper (headless=" ++ show headless ++ ", profile=" ++ profile ++ ")..."
      putStrLn $ "Fetching URL: " ++ url
      html <- runSession (chromeConfig headless) $ do
        result <- infiniteScroll url (pure ()) delaySec profile
        wins <- windows
        mapM_ closeWindow wins
        closeSession
        return result
      putStrLn "Successfully fetched HTML source."
      if T.null html
        then putStrLn "HTML content is empty, skipping scraping."
        else do
          putStrLn "Scraping products from HTML..."
          let products = scrapeStringLike (T.unpack html) scrapeProducts
          case products of
            Just prods -> do
              putStrLn $ "Successfully scraped " ++ show (length prods) ++ " products."
              BL.writeFile outputFile (encodePretty prods)
              putStrLn $ "Scraped data saved to " ++ outputFile
            Nothing -> putStrLn "Failed to scrape products."
    ScrapeNavLinks opts -> do
      let mOutputFile = navOutputFile opts
          allLinks = navAll opts
      putStrLn "Starting navigation link scraper..."
      navLinks <- scrapeNavLinks
      let filteredLinks = if allLinks then navLinks else filter ("/us/" `isInfixOf`) navLinks
      case mOutputFile of
        Just outputFile -> do
          BL.writeFile outputFile (encodePretty filteredLinks)
          putStrLn $ "Navigation links saved to " ++ outputFile
        Nothing -> BL.putStr (encodePretty filteredLinks)
