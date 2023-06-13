{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.OrderInteractionCreatedResponse
    ( orderInteractionId
    , OrderInteractionCreatedResponse ( OrderInteractionCreatedResponse )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data OrderInteractionCreatedResponse = OrderInteractionCreatedResponse
  { orderInteractionId :: Int
  } deriving (Generic)

instance Aeson.ToJSON OrderInteractionCreatedResponse where
    toJSON orderCreatedResponse = Aeson.object [ "id" Aeson..= orderInteractionId orderCreatedResponse]
