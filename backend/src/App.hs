{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App
    ( app
    ) where

import Web.Scotty
import qualified Data.Aeson as Aeson
import Database.MySQL.Simple
import GHC.Generics
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as TL

import qualified Security.Password as Password

data User = User
  { userId :: Int
  , name :: String
  , email :: String
  , password :: String
  } deriving (Show, Eq, Generic)

data CreateUserRequest = CreateUserRequest
  { name' :: String
  , email' :: String
  , password' :: String
  } deriving (Show, Eq, Generic)

data SignInRequest = SignInRequest
  { email'' :: String
  , password'' :: String
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON User
instance Aeson.ToJSON User

instance Aeson.FromJSON CreateUserRequest where
    parseJSON (Aeson.Object v) = CreateUserRequest
        <$> v Aeson..: "name"
        <*> v Aeson..: "email"
        <*> v Aeson..: "password"

instance Aeson.FromJSON SignInRequest where
    parseJSON (Aeson.Object v) = SignInRequest
        <$> v Aeson..: "email"
        <*> v Aeson..: "password"


saveUser :: Connection -> CreateUserRequest -> IO Int
saveUser conn user = do
    _ <- execute conn "INSERT INTO users (name, email, hashed_password) values (?, ?, ?)" (name' user, email' user, password' user)
    [Only lastReturnedId] <- query_ conn "SELECT LAST_INSERT_ID();"
    return lastReturnedId

getUserByEmail :: Connection -> String -> IO (Either String User)
getUserByEmail conn email = do
    rows <- query conn "SELECT * FROM users WHERE email = ?" (Only email :: Only String)
    case rows of
        [] -> return (Left "No user found")
        [(userId, name, email, password)] -> return (Right (User { userId = userId, name = name, email = email, password = password }))
        _ -> return (Left "Multiple users found")

app :: IO ()
app = do
    conn <- connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm" })

    scotty 3000 $ do
        post "/signup" $ do
            requestBody <- body
            let maybeUser = Aeson.decode requestBody :: Maybe CreateUserRequest
            case maybeUser of
                Nothing -> text "Invalid JSON"
                Just user -> do
                    hashedPassword <- Password.hashPassword (password' user)
                    let userWithHashedPassword = user { password' = TL.unpack hashedPassword }
                    userId <- liftIO (saveUser conn userWithHashedPassword)
                    text (TL.pack ("User created, id: " ++ show userId))

        post "/signin" $ do
            requestBody <- body
            let maybeSignInRequest = Aeson.decode requestBody :: Maybe SignInRequest
            case maybeSignInRequest of
                Nothing -> text "Invalid JSON"
                Just request -> do
                    user <- liftIO (getUserByEmail conn (email'' request))
                    case user of
                        Left error -> text $ TL.pack error
                        Right user ->
                            if Password.comparePassword (password user) (password'' request)
                                then text "OK"
                                else text "Not OK"


