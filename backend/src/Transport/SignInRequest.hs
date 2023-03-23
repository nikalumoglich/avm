{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON SignInRequest where
    parseJSON (Aeson.Object v) = SignInRequest
        <$> v Aeson..: "email"
        <*> v Aeson..: "password"