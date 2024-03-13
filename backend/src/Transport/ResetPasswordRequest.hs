{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.ResetPasswordRequest
    ( ResetPasswordRequest (..)
    ) where

import GHC.Generics
import Data.Aeson

data ResetPasswordRequest = ResetPasswordRequest
  { email :: String
  , code :: String
  , newPassword :: String
  } deriving (Generic)

instance FromJSON ResetPasswordRequest where
    parseJSON (Object v) = ResetPasswordRequest
        <$> v .: "email"
        <*> v .: "code"
        <*> v .: "newPassword"