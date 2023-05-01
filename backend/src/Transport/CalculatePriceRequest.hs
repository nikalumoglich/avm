{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.CalculatePriceRequest
    ( dimensionId
    , value
    , productId
    , dimensionValues
    , CalculatePriceRequest ( CalculatePriceRequest )
    , DimensionValue ( DimensionValue )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data DimensionValue = DimensionValue
  { dimensionId :: Int
  , value :: Float
  } deriving (Generic)

instance Aeson.FromJSON DimensionValue

data CalculatePriceRequest = CalculatePriceRequest
  { productId :: Int
  , dimensionValues :: [DimensionValue]
  } deriving (Generic)

instance Aeson.FromJSON CalculatePriceRequest