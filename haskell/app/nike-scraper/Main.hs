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

-- | Scroll to the bottom of the page using JavaScript
scrollToBottom :: WD ()
scrollToBottom = executeJS [] "window.scrollTo(0, document.body.scrollHeight);"

-- | Get the current scroll height of the page
getScrollHeight :: WD (Maybe Integer)
getScrollHeight = executeJS [] "return document.body.scrollHeight;"

-- | Wait for a specified number of milliseconds
waitMs :: Int -> WD ()
waitMs ms = liftIO $ threadDelay (ms * 1000)

-- | Perform infinite scrolling until no new content is loaded
-- Returns True if new content was loaded, False otherwise
performInfiniteScroll :: Integer -> Int -> WD Bool
performInfiniteScroll previousHeight maxAttempts
  | maxAttempts <= 0 = return False
  | otherwise = do
      -- Scroll to bottom
      scrollToBottom
      -- Wait for content to load
      waitMs 2000
      -- Get new scroll height
      newHeight <- getScrollHeight
      case newHeight of
        Nothing -> return False
        Just h -> 
          if h > previousHeight
            then do
              -- New content loaded, continue scrolling
              putStrLn $ "New content loaded. Previous height: " ++ show previousHeight ++ ", New height: " ++ show h
              performInfiniteScroll h (maxAttempts - 1)
            else do
              -- No new content, we're done
              putStrLn "No new content loaded. Infinite scrolling complete."
              return True

-- | Scroll through the entire page to load all products
scrollToLoadAllProducts :: WD ()
scrollToLoadAllProducts = do
  putStrLn "Starting infinite scrolling to load all products..."
  initialHeight <- getScrollHeight
  case initialHeight of
    Nothing -> putStrLn "Could not get initial scroll height"
    Just h -> do
      putStrLn $ "Initial page height: " ++ show h
      -- Try scrolling with a maximum of 50 attempts to prevent infinite loops
      success <- performInfiniteScroll h 50
      when (not success) $ putStrLn "Warning: Maximum scroll attempts reached"

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        -- Wait for initial page load
        waitMs 3000
        -- Perform infinite scrolling to load all products
        scrollToLoadAllProducts
        -- Get the final HTML source with all products loaded
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