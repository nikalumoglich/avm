{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.SignInRequest
    ( email
    , password
    , SignInRequest ( SignInRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data SignInRequest = SignInRequest
  { email :: String
  , password :: String
  } deriving (Generic)

instance Aeson.FromJSON SignInRequest where
    parseJSON (Aeson.Object v) = SignInRequest
        <$> v Aeson..: "email"
        <*> v Aeson..: "password"