{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.CreateOrderInteractionRequest
    ( orderId
    , text
    , imageIds
    , videoIds
    , CreateOrderInteractionRequest ( CreateOrderInteractionRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data CreateOrderInteractionRequest = CreateOrderInteractionRequest
  { orderId :: Int
  , text :: String
  , imageIds :: [Int]
  , videoIds :: [Int]
  } deriving (Generic)

instance Aeson.FromJSON CreateOrderInteractionRequest