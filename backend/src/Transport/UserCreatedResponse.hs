{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Transport.UserCreatedResponse
    ( userId
    , UserCreatedResponse ( UserCreatedResponse )
    ) where

import GHC.Generics
import qualified Data.Aeson as Aeson

data UserCreatedResponse = UserCreatedResponse
  { userId :: Int
  } deriving (Generic)

instance Aeson.ToJSON UserCreatedResponse where
    toJSON userCreatedResponse = Aeson.object [ "id" Aeson..= userId userCreatedResponse]
