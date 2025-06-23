{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts,
    countProducts,
    getTotalItemCount
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

-- Extract total item count from the header
getTotalItemCount :: String -> Maybe Int
getTotalItemCount html = do
    countText <- scrapeStringLike html $ text ("span" @: [hasClass "wall-header__item_count"])
    -- Extract number from "(639)" format
    let cleanedText = filter (`elem` ("0123456789" :: String)) countText
    readMaybe cleanedText 