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
import Data.Aeson

data CreateUserRequest = CreateUserRequest
  { name :: String
  , email :: String
  , password :: String
  } deriving (Generic)

instance FromJSON CreateUserRequest where
    parseJSON (Object v) = CreateUserRequest
        <$> v .: "name"
        <*> v .: "email"
        <*> v .: "password"
