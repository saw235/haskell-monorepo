{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Test.WebDriver.Commands
import Test.WebDriver.Monad (WD)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import qualified Data.Text.IO as TIO
import Test.WebDriver.Commands (getCurrentWindow, closeWindow, windows, closeSession)
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isPrefixOf, isInfixOf)
import qualified Options.Applicative as OA
import Nike.NavScraper (scrapeNavLinks)

-- | Configuration for Chrome WebDriver
chromeConfig :: Bool -> WDConfig
chromeConfig headless = useBrowser (chrome { chromeOptions = opts }) defaultConfig
  where
    userAgent = "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    opts = (if headless then ["--headless"] else []) ++ ["--no-sandbox", "--disable-dev-shm-usage", userAgent]

-- | CLI Options

data PageOptions = PageOptions
  { pageUrl        :: String
  , pageOutputFile :: FilePath
  }

data NavLinkOptions = NavLinkOptions
  { navOutputFile :: Maybe FilePath
  , navAll :: Bool
  }

data Command
  = ScrapePage PageOptions
  | ScrapeNavLinks NavLinkOptions

parsePageOptions :: OA.Parser PageOptions
parsePageOptions = PageOptions
  <$> OA.strOption
        ( OA.long "url"
       <> OA.metavar "URL"
       <> OA.help "The Nike product page URL to scrape" )
  <*> OA.strOption
        ( OA.long "output"
       <> OA.metavar "FILE"
       <> OA.help "The output file for the JSON data"
       <> OA.value "products.json"
       <> OA.showDefault )

parseNavLinkOptions :: OA.Parser NavLinkOptions
parseNavLinkOptions = NavLinkOptions
  <$> OA.optional (OA.strOption
        ( OA.long "output"
       <> OA.metavar "FILE"
       <> OA.help "The output file for the navigation links JSON" ))
  <*> OA.switch
        ( OA.long "all"
       <> OA.help "Include all links (not just /w/ links)" )

parseCommand :: OA.Parser Command
parseCommand = OA.hsubparser
  ( OA.command "scrape-page"
      (OA.info (ScrapePage <$> parsePageOptions)
        (OA.progDesc "Scrape Nike products from a given URL and output as JSON"))
 <> OA.command "scrape-nav-links"
      (OA.info (ScrapeNavLinks <$> parseNavLinkOptions)
        (OA.progDesc "Scrape all category URLs from the Nike navigation menu and output as JSON"))
  )

optsParserInfo :: OA.ParserInfo Command
optsParserInfo = OA.info (parseCommand OA.<**> OA.helper)
  ( OA.fullDesc <> OA.progDesc "Nike scraper CLI" )


-- | Patch browser JS environment to bypass headless detection
patchHeadlessDetection :: WD ()
patchHeadlessDetection = do
    _ <- (executeJS [] "Object.defineProperty(navigator, 'webdriver', {get: () => false});" :: WD ())
    _ <- (executeJS [] "Object.defineProperty(navigator, 'plugins', {get: () => [1,2,3,4,5]});" :: WD ())
    pure ()

-- | Count the number of product cards on the page
countProductCards :: WD Int
countProductCards = do
    elems <- findElems (ByCSS ".product-card")
    return (length elems)

-- | Scroll to the bottom of the page
scrollToBottom :: WD ()
scrollToBottom = do
    _ <- (executeJS [] "window.scrollTo(0, document.body.scrollHeight);" :: WD ())
    pure ()

-- | Perform infinite scroll until no new products are loaded
infiniteScroll :: String -> IO () -> WD T.Text
infiniteScroll url logAction = do
    openPage url
    patchHeadlessDetection
    let scrollLoop prevCount = do
            scrollToBottom
            liftIO $ threadDelay (3 * 1000000) -- 3 seconds
            currCount <- countProductCards
            liftIO $ logAction >> putStrLn ("Scrolled. Product count: " ++ show currCount)
            if currCount == prevCount || currCount == 0
              then getSource
              else scrollLoop currCount
    initialCount <- countProductCards
    liftIO $ logAction >> putStrLn ("Initial product count: " ++ show initialCount)
    scrollLoop initialCount

-- | Output the scraped products in the desired format
outputProducts :: ToJSON a => Bool -> Maybe FilePath -> [a] -> (a -> String) -> IO ()
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
            headless = False -- Default to non-headless mode; can be extended if needed

        putStrLn $ "Starting Nike scraper (headless=" ++ show headless ++ ")..."
        putStrLn $ "Fetching URL: " ++ url
        html <- runSession (chromeConfig headless) $ do
            result <- infiniteScroll url (pure ())
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
                Nothing    -> putStrLn "Failed to scrape products."
      ScrapeNavLinks opts -> do
        let mOutputFile = navOutputFile opts
            allLinks = navAll opts
        putStrLn "Starting navigation link scraper..."
        navLinks <- scrapeNavLinks
        let filteredLinks = if allLinks then navLinks else filter ("/w/" `isInfixOf`) navLinks
        case mOutputFile of
          Just outputFile -> do
            BL.writeFile outputFile (encodePretty filteredLinks)
            putStrLn $ "Navigation links saved to " ++ outputFile
          Nothing -> BL.putStr (encodePretty filteredLinks) 

