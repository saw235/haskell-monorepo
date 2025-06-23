{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)

chromeConfig :: WDConfig
chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- Scroll to bottom and wait for new content to load
scrollToBottom :: WD ()
scrollToBottom = do
    executeJS [] "window.scrollTo(0, document.body.scrollHeight);"
    -- Wait for content to load (2 seconds)
    liftIO $ threadDelay 2000000

-- Get current number of product cards on the page using multiple selectors
getProductCount :: WD Int
getProductCount = do
    result <- executeJS [] $ unlines [
        "var selectors = [",
        "  'div.product-card',",
        "  'article.product-card',", 
        "  '[data-testid=\"product-card\"]',",
        "  'div.grid-item',",
        "  '.product-tile',",
        "  '.product-item'",
        "];",
        "var maxCount = 0;",
        "for (var i = 0; i < selectors.length; i++) {",
        "  var count = document.querySelectorAll(selectors[i]).length;",
        "  if (count > maxCount) maxCount = count;",
        "}",
        "return maxCount;"
        ]
    return $ fromMaybe 0 result

-- Perform infinite scrolling until no new products are loaded
performInfiniteScrolling :: WD ()
performInfiniteScrolling = do
    putStrLnWD "Starting infinite scrolling..."
    go 0 0
  where
    go :: Int -> Int -> WD ()
    go prevCount attempts = do
        scrollToBottom
        currentCount <- getProductCount
        putStrLnWD $ "Products found: " ++ show currentCount ++ " (previous: " ++ show prevCount ++ ")"
        
        if currentCount > prevCount && attempts < 50  -- Limit to 50 attempts to prevent infinite loops
          then do
            putStrLnWD "New products detected, continuing to scroll..."
            go currentCount (attempts + 1)
          else do
            putStrLnWD $ "No new products found after " ++ show attempts ++ " scroll attempts. Finished scrolling."

-- Helper function to print in WebDriver context
putStrLnWD :: String -> WD ()
putStrLnWD = liftIO . putStrLn

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        -- Wait for initial page load
        liftIO $ threadDelay 3000000  -- 3 seconds
        
        -- Perform infinite scrolling to load all products
        performInfiniteScrolling
        
        -- Get final HTML after all scrolling
        finalHtml <- getSource
        putStrLnWD "Successfully completed infinite scrolling and retrieved final HTML."
        return finalHtml
    
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