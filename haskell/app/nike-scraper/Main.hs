{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts, countProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import Test.WebDriver.JSON
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- Scroll to bottom and wait for content to load
scrollToBottom :: WD ()
scrollToBottom = do
    _ <- executeJS [] "window.scrollTo(0, document.body.scrollHeight);" :: WD (Maybe ())
    -- Wait 2 seconds for content to load
    liftIO $ threadDelay 2000000  -- 2 seconds in microseconds

-- Get current product count from the page
getCurrentProductCount :: WD Int
getCurrentProductCount = do
    html <- getSource
    return $ countProducts (T.unpack html)

-- Keep scrolling until no new products load or max attempts reached
scrollUntilComplete :: Int -> WD ()
scrollUntilComplete maxAttempts = scrollUntilCompleteHelper maxAttempts 0

scrollUntilCompleteHelper :: Int -> Int -> WD ()
scrollUntilCompleteHelper maxAttempts attempts = do
    when (attempts < maxAttempts) $ do
        initialCount <- getCurrentProductCount
        liftIO $ putStrLn $ "Current product count: " ++ show initialCount ++ " (attempt " ++ show (attempts + 1) ++ "/" ++ show maxAttempts ++ ")"
        scrollToBottom
        newCount <- getCurrentProductCount
        
        if newCount > initialCount
            then do
                liftIO $ putStrLn $ "New products loaded! Count increased from " ++ show initialCount ++ " to " ++ show newCount
                scrollUntilCompleteHelper maxAttempts (attempts + 1)
            else do
                liftIO $ putStrLn "No new products loaded, infinite scroll complete."

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling support..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        liftIO $ putStrLn "Waiting for initial page load..."
        liftIO $ threadDelay 3000000  -- Wait 3 seconds for initial load
        
        liftIO $ putStrLn "Starting infinite scroll process..."
        scrollUntilComplete 10  -- Maximum 10 scroll attempts
        liftIO $ putStrLn "Infinite scroll complete, getting final HTML..."
        
        getSource

    putStrLn "Successfully fetched HTML source after infinite scrolling."

    if T.null html
      then putStrLn "HTML content is empty, skipping scraping."
      else do
        putStrLn "Scraping products from HTML..."
        let products = scrapeStringLike (T.unpack html) scrapeProducts
        case products of
            Just prods -> do
                putStrLn $ "Successfully scraped " ++ show (length prods) ++ " products."
                mapM_ print prods
            Nothing    -> putStrLn "Failed to scrape products." 