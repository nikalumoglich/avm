{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.RecoveryCode
    ( saveRecoveryCode
    , getActiveRecoveryCode
    , inactivateRecoveryCode
    , RecoveryCode (..)
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Model.User as User

data RecoveryCode = RecoveryCode
  { recoveryCodeId :: Int
  , userId :: Int
  , hashedCode :: String
  , expiration :: Int
  , status :: String
  } | RecoveryCodeNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON RecoveryCode
instance Aeson.ToJSON RecoveryCode

saveRecoveryCode :: Connection -> User.User -> String -> Int -> IO Int
saveRecoveryCode conn user hashedRecoveryCode expiration = do
    _ <- execute conn "INSERT INTO `recovery_code` (user_id, hashed_code, expiration, status) values (?, ?, ?, ?)" (User.userId user, hashedRecoveryCode, expiration, "active" :: String)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return lastReturnedId

getActiveRecoveryCode conn userId = do
    rows <- query conn "SELECT `id`, `hashed_code` FROM `recovery_code` WHERE user_id = ? AND status = ? AND expiration > UNIX_TIMESTAMP() ORDER BY expiration DESC LIMIT 1" (userId, "active" :: String)
    case rows of
        [] -> return RecoveryCodeNotFound
        ((id, hashedCode):_) -> return (RecoveryCode { recoveryCodeId = id, userId = userId, hashedCode = hashedCode, expiration = 0, status = "active" })

inactivateRecoveryCode conn recoveryCodeId = do
  execute conn "UPDATE `recovery_code` SET `status` = ? WHERE id = ?" ("inactive" :: String, recoveryCodeId)