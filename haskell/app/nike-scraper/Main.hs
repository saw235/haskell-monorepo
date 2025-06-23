{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Control.Concurrent (threadDelay)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- Function to perform infinite scrolling until no new products are loaded
performInfiniteScrolling :: WD ()
performInfiniteScrolling = do
    putStrLn "Starting infinite scrolling..."
    initialProductCount <- countProducts
    putStrLn $ "Initial product count: " ++ show initialProductCount
    scrollUntilNoNewProducts initialProductCount 0
  where
    -- Count the number of product cards currently visible
    countProducts :: WD Int
    countProducts = do
        elements <- findElems (ByCSS "div.product-card")
        return $ length elements
    
    -- Scroll down and check for new products, with a retry limit
    scrollUntilNoNewProducts :: Int -> Int -> WD ()
    scrollUntilNoNewProducts lastCount retryCount
        | retryCount >= 3 = do
            putStrLn $ "No new products loaded after 3 attempts. Final count: " ++ show lastCount
            return ()
        | otherwise = do
            -- Scroll to bottom
            executeJS [] "window.scrollTo(0, document.body.scrollHeight);" :: WD (Maybe ())
            
            -- Wait for content to load
            putStrLn "Scrolled to bottom, waiting for content to load..."
            threadDelay (2000000) -- Wait 2 seconds
            
            -- Count products again
            newCount <- countProducts
            putStrLn $ "Product count after scrolling: " ++ show newCount
            
            if newCount > lastCount
                then do
                    putStrLn $ "Found " ++ show (newCount - lastCount) ++ " new products, continuing..."
                    scrollUntilNoNewProducts newCount 0 -- Reset retry count when new products found
                else do
                    putStrLn $ "No new products found (attempt " ++ show (retryCount + 1) ++ "/3)"
                    scrollUntilNoNewProducts lastCount (retryCount + 1)

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        
        -- Wait for initial page load
        putStrLn "Waiting for initial page load..."
        threadDelay (3000000) -- Wait 3 seconds
        
        -- Perform infinite scrolling
        performInfiniteScrolling
        
        -- Get final HTML after all scrolling
        putStrLn "Getting final HTML after scrolling..."
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