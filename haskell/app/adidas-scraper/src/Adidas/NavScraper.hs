{-# LANGUAGE OverloadedStrings #-}

module Adidas.NavScraper (
    scrapeNavLinks
) where

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.Commands.Wait
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

adidasHomeUrl :: String
adidasHomeUrl = "https://www.adidas.com/us"

-- | Scrape all navigation menu links from the Adidas homepage
scrapeNavLinks :: IO [String]
scrapeNavLinks = runSession (useBrowser chrome defaultConfig) $ do
    openPage adidasHomeUrl
    -- Wait for the nav menu to load
    _ <- waitUntil 10000 $ findElem (ByCSS "nav[data-testid='desktop-menu-container']")
    navElem <- findElem (ByCSS "nav[data-testid='desktop-menu-container']")
    -- Find all top-level menu items (li > div)
    topLevelDivs <- findElemsFrom navElem (ByCSS "ul.desktop-category > li > div")
    -- Hover over each top-level menu item to reveal dropdowns
    mapM_ moveToCenter topLevelDivs
    liftIO $ putStrLn $ "Hovered over " ++ show (length topLevelDivs) ++ " top-level menu items."
    -- Wait a bit for dropdowns to appear
    liftIO $ threadDelay (2 * 1000000)
    -- Find all <a> elements within the nav
    linkElems <- findElemsFrom navElem (ByCSS "a[data-testid='link']")
    hrefs <- mapM (\elem -> attr elem "href") linkElems
    let scrapedUrls = Set.fromList $ map T.unpack $ catMaybes hrefs
        -- Add some common Adidas category URLs
        commonUrls = [ "https://www.adidas.com/us/men"
                    , "https://www.adidas.com/us/women" 
                    , "https://www.adidas.com/us/kids"
                    , "https://www.adidas.com/us/sport"
                    ]
        urls = Set.toList $ Set.union scrapedUrls (Set.fromList commonUrls)
    closeSession
    return urls 