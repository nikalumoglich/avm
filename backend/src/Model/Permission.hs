{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Model.Permission
    ( permission
    , getUserPermissions
    , Permission ( Permission )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import Control.Monad
import qualified Data.Aeson as Aeson

data Permission = Permission
  { permission :: String
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON Permission
instance Aeson.ToJSON Permission

getUserPermissions :: Connection -> Int -> IO [Permission]
getUserPermissions conn userId = do
    permissions <- query conn "SELECT permission FROM users \
    \                   INNER JOIN users_permissions ON users.id = users_permissions.user_id \
    \                   INNER JOIN permissions ON permissions.id = users_permissions.permission_id \
    \                   WHERE users.id = ?" (Only (userId :: Int))
    forM permissions $ \(Only permission) -> return Permission { permission = permission }