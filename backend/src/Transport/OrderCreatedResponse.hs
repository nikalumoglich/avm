{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.OrderCreatedResponse
    ( orderId
    , OrderCreatedResponse ( OrderCreatedResponse )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data OrderCreatedResponse = OrderCreatedResponse
  { orderId :: Int
  } deriving (Generic)

instance Aeson.ToJSON OrderCreatedResponse where
    toJSON orderCreatedResponse = Aeson.object [ "id" Aeson..= orderId orderCreatedResponse]
