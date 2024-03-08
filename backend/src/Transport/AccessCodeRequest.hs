{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.AccessCodeRequest
    ( email
    , AccessCodeRequest ( AccessCodeRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data AccessCodeRequest = AccessCodeRequest
  { email :: String
  } deriving (Generic)

instance Aeson.FromJSON AccessCodeRequest where
    parseJSON (Aeson.Object v) = AccessCodeRequest
        <$> v Aeson..: "email"