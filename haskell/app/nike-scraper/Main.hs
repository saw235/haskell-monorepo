{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

-- Function to get the current scroll height of the page
getScrollHeight :: WD Integer
getScrollHeight = do
    result <- executeJS [] "return document.body.scrollHeight;"
    case result of
        Just (JSNumber height) -> return $ round height
        _ -> return 0

-- Function to scroll to the bottom of the page
scrollToBottom :: WD ()
scrollToBottom = do
    executeJS [] "window.scrollTo(0, document.body.scrollHeight);"
    return ()

-- Function to implement infinite scrolling
infiniteScroll :: Int -> Integer -> WD ()
infiniteScroll maxScrolls previousHeight = do
    if maxScrolls <= 0
        then return () -- Stop if we've reached max scrolls
        else do
            -- Scroll to bottom
            scrollToBottom
            -- Wait for content to load
            waitFor 2000  -- Wait 2 seconds for content to load
            -- Get new height
            newHeight <- getScrollHeight
            -- If height hasn't changed, we've reached the end
            if newHeight == previousHeight
                then return () -- No new content loaded, stop scrolling
                else infiniteScroll (maxScrolls - 1) newHeight

main :: IO ()
main = do
    putStrLn "Starting Nike scraper with infinite scrolling..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1zy7ok"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    
    result <- try $ runSession chromeConfig $ do
        openPage nikeUrl
        -- Wait for initial page load
        waitFor 3000
        
        -- Get initial scroll height
        initialHeight <- getScrollHeight
        liftIO $ putStrLn $ "Initial page height: " ++ show initialHeight
        
        -- Perform infinite scrolling (max 20 scrolls to prevent infinite loops)
        infiniteScroll 20 initialHeight
        liftIO $ putStrLn "Finished scrolling, getting final page source..."
        
        -- Get the final page source after all scrolling
        getSource
    
    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error during web scraping: " ++ show e
            putStrLn "This might be due to network issues or the page structure changing."
        Right html -> do
            putStrLn "Successfully fetched HTML source."
            
            if T.null html
                then putStrLn "HTML content is empty, skipping scraping."
                else do
                    putStrLn "Scraping products from HTML..."
                    let products = scrapeStringLike (T.unpack html) scrapeProducts
                    case products of
                        Just prods -> do
                            putStrLn $ "Successfully scraped " ++ show (length prods) ++ " products."
                            mapM_ print prods
                        Nothing    -> putStrLn "Failed to scrape products. The page structure might have changed." 