{-# LANGUAGE OverloadedStrings #-}

module Nike.NavScraper (
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

nikeHomeUrl :: String
nikeHomeUrl = "https://www.nike.com/"

-- | Scrape all navigation menu links from the Nike homepage
scrapeNavLinks :: IO [String]
scrapeNavLinks = runSession (useBrowser chrome defaultConfig) $ do
    openPage nikeHomeUrl
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
    let urls = Set.toList $ Set.fromList $ map T.unpack $ catMaybes hrefs
    closeSession
    return urls 