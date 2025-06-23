{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts, countProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Control.Concurrent (threadDelay)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- Scroll to bottom and wait for content to load
scrollToBottom :: WD ()
scrollToBottom = do
    executeJS [] "window.scrollTo(0, document.body.scrollHeight);"
    -- Wait 2 seconds for content to load
    liftIO $ threadDelay 2000000

-- Count products on the current page
countCurrentProducts :: WD Int
countCurrentProducts = do
    html <- getSource
    let productCount = scrapeStringLike (T.unpack html) countProducts
    return $ maybe 0 id productCount

-- Keep scrolling until no new products are loaded
scrollUntilComplete :: Int -> WD ()
scrollUntilComplete previousCount = do
    liftIO $ putStrLn $ "Current product count: " ++ show previousCount
    scrollToBottom
    newCount <- countCurrentProducts
    liftIO $ putStrLn $ "After scroll product count: " ++ show newCount
    
    if newCount > previousCount
        then do
            liftIO $ putStrLn "New products loaded, continuing to scroll..."
            scrollUntilComplete newCount
        else do
            liftIO $ putStrLn "No new products loaded, scrolling complete."

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling support..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        liftIO $ putStrLn "Page loaded, starting infinite scroll..."
        
        -- Get initial product count
        initialCount <- countCurrentProducts
        liftIO $ putStrLn $ "Initial product count: " ++ show initialCount
        
        -- Scroll until no new products are loaded
        scrollUntilComplete initialCount
        
        -- Get final HTML after all scrolling is complete
        liftIO $ putStrLn "Getting final HTML source..."
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