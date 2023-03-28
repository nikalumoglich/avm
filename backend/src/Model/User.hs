{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Model.User
    ( userId
    , name
    , email
    , password
    , saveUser
    , getUserByEmail
    , User ( User, UserNotFound )
    ) where

import GHC.Generics
import Database.MySQL.Simple
import qualified Data.Aeson as Aeson
import qualified Transport.CreateUserRequest as CreateUserRequest

data User = User
  { userId :: Int
  , name :: String
  , email :: String
  , password :: String
  } | UserNotFound
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON User
instance Aeson.ToJSON User

saveUser :: Connection -> CreateUserRequest.CreateUserRequest -> IO (Maybe Int)
saveUser conn user = do
    existingUser <- getUserByEmail conn $ CreateUserRequest.email user
    case existingUser of
        UserNotFound -> do
            _ <- execute conn "INSERT INTO users (name, email, hashed_password) values (?, ?, ?)" (CreateUserRequest.name user, CreateUserRequest.email user, CreateUserRequest.password user)
            [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
            return lastReturnedId
        _ -> return Nothing

getUserByEmail :: Connection -> String -> IO User
getUserByEmail conn email = do
    rows <- query conn "SELECT * FROM users WHERE email = ?" (Only email :: Only String)
    case rows of
        [] -> return UserNotFound
        (userId, name, email', password):_ -> return (User { userId = userId, name = name, email = email', password = password })