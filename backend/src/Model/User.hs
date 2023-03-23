{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Model.User
    ( userId
    , name
    , email
    , password
    , saveUser
    , getUserByEmail
    , User ( User )
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
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON User
instance Aeson.ToJSON User

saveUser :: Connection -> CreateUserRequest.CreateUserRequest -> IO Int
saveUser conn user = do
    _ <- execute conn "INSERT INTO users (name, email, hashed_password) values (?, ?, ?)" (CreateUserRequest.name user, CreateUserRequest.email user, CreateUserRequest.password user)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return lastReturnedId

getUserByEmail :: Connection -> String -> IO (Either String User)
getUserByEmail conn email = do
    rows <- query conn "SELECT * FROM users WHERE email = ?" (Only email :: Only String)
    case rows of
        [] -> return (Left "No user found")
        [(userId, name, email, password)] -> return (Right (User { userId = userId, name = name, email = email, password = password }))
        _ -> return (Left "Multiple users found")