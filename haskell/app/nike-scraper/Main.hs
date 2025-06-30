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
import Data.List (isPrefixOf)

-- | Configuration for Chrome WebDriver
chromeConfig :: Bool -> WDConfig
chromeConfig headless = useBrowser (chrome { chromeOptions = opts }) defaultConfig
  where
    userAgent = "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    opts = (if headless then ["--headless"] else []) ++ ["--no-sandbox", "--disable-dev-shm-usage", userAgent]

-- | Parse CLI arguments for headless, json, output path, and target URL
parseArgs :: [String] -> (Bool, Bool, Maybe FilePath, Maybe String)
parseArgs args =
    let headless = "--headless" `elem` args
        jsonOut = "--json" `elem` args
        mOutputPath = case dropWhile (/= "--output") args of
                        (_:path:_) -> Just path
                        _          -> Nothing
        nonFlagArgs = filter (\a -> not (a `elem` ["--no-headless", "--json"]) && not ("--output" `isPrefixOf` a)) args
        mUrl = case filter (\a -> "https://www.nike.com/w/" `isPrefixOf` a) nonFlagArgs of
                 (url:_) -> Just url
                 _       -> Nothing
    in (headless, jsonOut, mOutputPath, mUrl)

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
    args <- getArgs
    let (headless, jsonOut, mOutputPath, mUrl) = parseArgs args
    case mUrl of
      Nothing -> do
        putStrLn "Error: Please provide a Nike URL in the format https://www.nike.com/w/<category> as a positional argument."
        putStrLn "Example: bazel run //haskell/app/nike-scraper -- https://www.nike.com/w/mens-clothing-6ymx6znik1"
      Just url -> do
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
                    outputProducts jsonOut mOutputPath prods show
                Nothing    -> putStrLn "Failed to scrape products." 