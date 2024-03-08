{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.RecoveryCode
    ( recoveryCodeId
    , userId
    , hashedCode
    , expiration
    , status
    , saveRecoveryCode
    , RecoveryCode ( RecoveryCode )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Model.User as User

data RecoveryCode = RecoveryCode
  { recoveryCodeId :: Int
  , userId :: Int
  , hashedCode :: Int
  , expiration :: Int
  , status :: String
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON RecoveryCode
instance Aeson.ToJSON RecoveryCode

saveRecoveryCode :: Connection -> User.User -> String -> Int -> IO Int
saveRecoveryCode conn user hashedRecoveryCode expiration = do
    _ <- execute conn "INSERT INTO recovery_code (user_id, hashed_code, expiration, status) values (?, ?, ?, ?)" (User.userId user, hashedRecoveryCode, expiration, "active" :: String)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return lastReturnedId
