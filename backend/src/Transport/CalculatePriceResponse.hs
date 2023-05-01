{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Transport.CalculatePriceResponse
    ( value
    , CalculatePriceResponse ( CalculatePriceResponse )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data CalculatePriceResponse = CalculatePriceResponse
  { value :: Float
  } deriving (Generic)

instance Aeson.ToJSON CalculatePriceResponse