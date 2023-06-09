{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.CalculatePriceRequest
    ( productId
    , dimensionValues
    , CalculatePriceRequest ( CalculatePriceRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Controller.ProductController as ProductController

data CalculatePriceRequest = CalculatePriceRequest
  { productId :: Int
  , dimensionValues :: [ProductController.DimensionValue]
  } deriving (Generic)

instance Aeson.FromJSON CalculatePriceRequest