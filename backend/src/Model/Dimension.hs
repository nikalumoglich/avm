{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.Dimension
    ( dimensionId
    , productId
    , name
    , symbol
    , images
    , getDimensionsByProductId
    , Dimension ( Dimension )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Model.Image as Image

data Dimension = Dimension
  { dimensionId :: Int
  , productId :: Int
  , name :: String
  , symbol :: String
  , images :: [Image.Image]
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON Dimension
instance Aeson.ToJSON Dimension

rowToDimension :: (Int, Int, String, String) -> Dimension
rowToDimension (dimensionId, productId, name, symbol) = Dimension { dimensionId = dimensionId, productId = productId, name = name, symbol = symbol }

getDimensionsByProductId :: Connection -> String -> Int -> IO [Dimension]
getDimensionsByProductId conn bucket productId = do
    rows <- query conn "SELECT * FROM dimensions WHERE product_id = ?" (Only productId :: Only Int)
    let dimensions = map rowToDimension rows
    mapM (\dimension -> do
        images <- Image.getImagesByDimensionId conn bucket (dimensionId dimension)
        return dimension { images = images }
        ) dimensions
