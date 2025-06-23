{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- | Scroll to the bottom of the page and wait for content to load
scrollToBottom :: WD ()
scrollToBottom = do
    _ <- executeScript [] "window.scrollTo(0, document.body.scrollHeight);"
    -- Wait for new content to load (2 seconds)
    liftIO $ threadDelay 2000000

-- | Get the current number of product cards on the page
getProductCount :: WD Int
getProductCount = do
    elements <- findElems (ByCSS "div.product-card")
    return $ length elements

-- | Perform infinite scrolling until no new products are loaded
infiniteScroll :: Int -> Int -> WD ()
infiniteScroll previousCount maxAttempts
    | maxAttempts <= 0 = do
        liftIO $ putStrLn "Maximum scroll attempts reached."
        return ()
    | otherwise = do
        scrollToBottom
        currentCount <- getProductCount
        liftIO $ putStrLn $ "Products found: " ++ show currentCount
        
        if currentCount > previousCount
            then do
                liftIO $ putStrLn "New products loaded, continuing to scroll..."
                infiniteScroll currentCount 5  -- Reset attempts when new products are found
            else do
                liftIO $ putStrLn $ "No new products loaded. Attempts remaining: " ++ show (maxAttempts - 1)
                infiniteScroll currentCount (maxAttempts - 1)

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        
        -- Wait for initial page load
        liftIO $ threadDelay 3000000  -- 3 seconds
        
        -- Get initial product count
        initialCount <- getProductCount
        liftIO $ putStrLn $ "Initial products found: " ++ show initialCount
        
        -- Perform infinite scrolling
        when (initialCount > 0) $ do
            liftIO $ putStrLn "Starting infinite scroll to load all products..."
            infiniteScroll initialCount 5  -- Allow 5 consecutive attempts with no new products
        
        -- Get final HTML after all scrolling is complete
        finalCount <- getProductCount
        liftIO $ putStrLn $ "Final products count after scrolling: " ++ show finalCount
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