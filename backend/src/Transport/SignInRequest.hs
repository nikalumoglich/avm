{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.SignInRequest
    ( email
    , password
    , SignInRequest ( SignInRequest )
    ) where

import GHC.Generics
import Data.Aeson

data SignInRequest = SignInRequest
  { email :: String
  , password :: String
  } deriving (Generic)

instance FromJSON SignInRequest where
    parseJSON (Object v) = SignInRequest
        <$> v .: "email"
        <*> v .: "password"