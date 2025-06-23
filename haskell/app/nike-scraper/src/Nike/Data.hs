{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Nike.Data (
    Product(..),
    ApiResponse(..),
    ProductContainer(..),
    ApiProduct(..),
    Price(..)
) where

import GHC.Generics
import Data.Aeson

data Product = Product {
    productName  :: String,
    productPrice :: String
} deriving (Show, Eq)

data ApiResponse = ApiResponse {
    objects :: [ProductContainer]
} deriving (Show, Generic)

instance FromJSON ApiResponse

data ProductContainer = ProductContainer {
    product :: ApiProduct
} deriving (Show, Generic)

instance FromJSON ProductContainer

data ApiProduct = ApiProduct {
    title :: String,
    price :: Price
} deriving (Show, Generic)

instance FromJSON ApiProduct

data Price = Price {
    currentPrice :: Double
} deriving (Show, Generic)

instance FromJSON Price 