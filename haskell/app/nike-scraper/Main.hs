{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nike.Scraper (scrapeProducts)
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Test.WebDriver
import qualified Data.Text as T
import Control.Monad (when)

chromeConfig = useBrowser chromeHeadless defaultConfig
  where chromeHeadless = chrome { chromeOptions = ["--headless", "--no-sandbox", "--disable-dev-shm-usage"] }

main :: IO ()
main = do
    putStrLn "Starting Nike scraper..."
    let nikeUrl = "https://www.nike.com/w/mens-shoes-nik1-z5e1x6znik1"
    putStrLn $ "Fetching URL: " ++ nikeUrl
    html <- runSession chromeConfig $ do
        openPage nikeUrl
        getSource
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
            Nothing    -> putStrLn "Failed to scrape products." 