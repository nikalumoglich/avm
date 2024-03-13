{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.AccessCodeRequest
    ( email
    , AccessCodeRequest ( AccessCodeRequest )
    ) where

import GHC.Generics
import Data.Aeson

data AccessCodeRequest = AccessCodeRequest
  { email :: String
  } deriving (Generic)

instance FromJSON AccessCodeRequest where
    parseJSON (Object v) = AccessCodeRequest
        <$> v .: "email"