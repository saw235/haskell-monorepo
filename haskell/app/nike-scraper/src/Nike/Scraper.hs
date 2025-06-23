{-# LANGUAGE OverloadedStrings #-}

module Nike.Scraper (
    scrapeProducts
) where

import Text.HTML.Scalpel
import Nike.Data

scrapeProducts :: Scraper String [Product]
scrapeProducts = chroots ("div") $ do
    text' <- text "div"
    return $ Product text' "N/A" 