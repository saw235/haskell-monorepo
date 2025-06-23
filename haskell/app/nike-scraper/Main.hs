{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts, countProducts, getTotalItemCount, debugTotalCount)
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
    -- Wait 7 seconds for content to load (increased for Nike's slower loading)
    liftIO $ threadDelay 7000000  -- 7 seconds in microseconds

-- Get current product count from the page
getCurrentProductCount :: WD Int
getCurrentProductCount = do
    html <- getSource
    return $ countProducts (T.unpack html)

-- Get the expected total item count from the header
getExpectedTotal :: WD (Maybe Int)
getExpectedTotal = do
    html <- getSource
    liftIO $ debugTotalCount (T.unpack html)  -- Debug what we're extracting
    return $ getTotalItemCount (T.unpack html)

-- Keep scrolling until we have all expected products or max attempts reached
scrollUntilComplete :: Int -> WD ()
scrollUntilComplete maxAttempts = do
    expectedTotal <- getExpectedTotal
    case expectedTotal of
        Just total -> do
            liftIO $ putStrLn $ "Found expected total: " ++ show total ++ " products"
            scrollUntilCompleteWithTotal maxAttempts 0 total
        Nothing -> do
            liftIO $ putStrLn "Could not determine total count, using fallback method"
            scrollUntilCompleteHelper maxAttempts 0

scrollUntilCompleteWithTotal :: Int -> Int -> Int -> WD ()
scrollUntilCompleteWithTotal maxAttempts attempts expectedTotal = do
    when (attempts < maxAttempts) $ do
        currentCount <- getCurrentProductCount
        liftIO $ putStrLn $ "Progress: " ++ show currentCount ++ "/" ++ show expectedTotal ++ " products (attempt " ++ show (attempts + 1) ++ "/" ++ show maxAttempts ++ ")"
        
        if currentCount >= expectedTotal
            then liftIO $ putStrLn "All products loaded successfully!"
            else do
                scrollToBottom
                newCount <- getCurrentProductCount
                
                if newCount > currentCount
                    then do
                        liftIO $ putStrLn $ "New products loaded! Count increased from " ++ show currentCount ++ " to " ++ show newCount
                        liftIO $ putStrLn "Waiting a bit more to ensure all content is loaded..."
                        liftIO $ threadDelay 4000000  -- Additional 4 seconds after new content detected
                        scrollUntilCompleteWithTotal maxAttempts (attempts + 1) expectedTotal
                    else do
                        liftIO $ putStrLn $ "No new products loaded. Current: " ++ show newCount ++ ", Expected: " ++ show expectedTotal
                        if newCount < expectedTotal
                            then do
                                liftIO $ putStrLn "Still missing products, trying one more scroll..."
                                scrollUntilCompleteWithTotal maxAttempts (attempts + 1) expectedTotal
                            else liftIO $ putStrLn "Infinite scroll complete."

-- Fallback method for when we can't get total count
scrollUntilCompleteHelper :: Int -> Int -> WD ()
scrollUntilCompleteHelper maxAttempts attempts = 
    scrollUntilCompleteHelperWithRetry maxAttempts attempts 0

scrollUntilCompleteHelperWithRetry :: Int -> Int -> Int -> WD ()
scrollUntilCompleteHelperWithRetry maxAttempts attempts noProgressCount = do
    when (attempts < maxAttempts && noProgressCount < 2) $ do  -- Allow up to 2 consecutive no-progress attempts
        initialCount <- getCurrentProductCount
        liftIO $ putStrLn $ "Current product count: " ++ show initialCount ++ " (attempt " ++ show (attempts + 1) ++ "/" ++ show maxAttempts ++ ", no-progress streak: " ++ show noProgressCount ++ ")"
        scrollToBottom
        newCount <- getCurrentProductCount
        
        if newCount > initialCount
            then do
                liftIO $ putStrLn $ "New products loaded! Count increased from " ++ show initialCount ++ " to " ++ show newCount
                liftIO $ putStrLn "Waiting a bit more to ensure all content is loaded..."
                liftIO $ threadDelay 4000000  -- Additional 4 seconds after new content detected
                scrollUntilCompleteHelperWithRetry maxAttempts (attempts + 1) 0  -- Reset no-progress count
            else do
                liftIO $ putStrLn $ "No new products loaded this attempt. Will try " ++ show (1 - noProgressCount) ++ " more times before giving up."
                scrollUntilCompleteHelperWithRetry maxAttempts (attempts + 1) (noProgressCount + 1)
                
    when (noProgressCount >= 2) $ 
        liftIO $ putStrLn "No progress after 2 consecutive attempts, infinite scroll complete."

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling support..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        liftIO $ putStrLn "Waiting for initial page load..."
        liftIO $ threadDelay 8000000  -- Wait 8 seconds for initial load (increased for dynamic content)
        
        liftIO $ putStrLn "Checking for total count after initial load..."
        expectedTotal1 <- getExpectedTotal
        case expectedTotal1 of
            Just total -> liftIO $ putStrLn $ "Found total count on first try: " ++ show total
            Nothing -> do
                liftIO $ putStrLn "Total count not found yet, waiting more..."
                liftIO $ threadDelay 3000000  -- Wait another 3 seconds
        
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