module Nike.Data (
    Product(..)
) where

data Product = Product {
    productName  :: String,
    productPrice :: String
} deriving (Show, Eq) 