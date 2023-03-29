{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.CreateUserRequest
    ( name
    , email
    , password
    , CreateUserRequest ( CreateUserRequest )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data CreateUserRequest = CreateUserRequest
  { name :: String
  , email :: String
  , password :: String
  } deriving (Generic)

instance Aeson.FromJSON CreateUserRequest where
    parseJSON (Aeson.Object v) = CreateUserRequest
        <$> v Aeson..: "name"
        <*> v Aeson..: "email"
        <*> v Aeson..: "password"
