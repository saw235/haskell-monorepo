{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts,
    countProducts,
    getTotalItemCount,
    debugTotalCount
) where

import Text.HTML.Scalpel
import Nike.Data
import qualified Data.Text as T
import Text.Read (readMaybe)

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div" @: [hasClass "product-card"]) $ do
    name <- text ("div" @: [hasClass "product-card__title"])
    price <- text ("div" @: [hasClass "product-price"])
    imageUrl <- attr "src" ("img" @: [hasClass "product-card__hero-image"])
    return $ Product name price imageUrl

-- Count the number of product cards on the page
countProducts :: String -> Int
countProducts html = 
    case scrapeStringLike html (length <$> chroots ("div" @: [hasClass "product-card"]) (return ())) of
        Just count -> count
        Nothing    -> 0

-- Extract total item count from the header using multiple approaches
getTotalItemCount :: String -> Maybe Int
getTotalItemCount html = 
    -- Try the primary selector first
    trySpanSelector `orElse` tryH1Selector `orElse` tryAnyNumberInHeader
  where
    trySpanSelector = do
        countText <- scrapeStringLike html $ text ("span" @: [hasClass "wall-header__item_count"])
        extractNumber countText
    
    tryH1Selector = do
        h1Text <- scrapeStringLike html $ text ("h1" @: [hasClass "wall-header__title"])
        extractNumber h1Text
    
    tryAnyNumberInHeader = do
        headerText <- scrapeStringLike html $ text ("header" @: [hasClass "wall-header"])
        extractNumber headerText
    
    extractNumber text =
        let cleanedText = filter (`elem` ("0123456789" :: String)) text
        in if null cleanedText then Nothing else readMaybe cleanedText
    
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y  = y

-- Debug function to see what we're actually extracting
debugTotalCount :: String -> IO ()
debugTotalCount html = do
    putStrLn "=== Debugging total count extraction ==="
    
    -- Try span selector
    let spanResult = scrapeStringLike html $ text ("span" @: [hasClass "wall-header__item_count"])
    putStrLn $ "Span selector result: " ++ show spanResult
    
    -- Try h1 selector  
    let h1Result = scrapeStringLike html $ text ("h1" @: [hasClass "wall-header__title"])
    putStrLn $ "H1 selector result: " ++ show h1Result
    
    -- Try header selector
    let headerResult = scrapeStringLike html $ text ("header" @: [hasClass "wall-header"])
    putStrLn $ "Header selector result: " ++ show headerResult
    
    -- Original logic
    let countText = scrapeStringLike html $ text ("span" @: [hasClass "wall-header__item_count"])
    putStrLn $ "Debug - countText extracted: " ++ show countText
    case countText of
        Just text -> do
            let cleanedText = filter (`elem` ("0123456789" :: String)) text
            putStrLn $ "Debug - cleanedText: " ++ show cleanedText
            putStrLn $ "Debug - readMaybe result: " ++ show (readMaybe cleanedText :: Maybe Int)
        Nothing -> putStrLn "Debug - No text found with the selector"
    putStrLn "=======================================" 