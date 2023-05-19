{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.CreateOrderRequest
    ( productId
    , dimensionValues
    , CreateOrderRequest ( CreateOrderRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Controller.ProductController as ProductController

data CreateOrderRequest = CreateOrderRequest
  { productId :: Int
  , dimensionValues :: [ProductController.DimensionValue]
  } deriving (Generic)

instance Aeson.FromJSON CreateOrderRequest