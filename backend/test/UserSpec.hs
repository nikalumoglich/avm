{-# LANGUAGE OverloadedStrings #-}

module UserSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import Model.User
import qualified Transport.CreateUserRequest as CreateUserRequest

createDbConn :: IO Connection
createDbConn = connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm_test" })

suiteSpec :: Spec
suiteSpec = do
  describe "UserSpec" $ do

    it "saveUser should save" $ do
      dbConn <- createDbConn
      _ <- execute dbConn "TRUNCATE TABLE users" ()
      saveUser dbConn (CreateUserRequest.CreateUserRequest "name" "email" "password") >>= (`shouldBe` Just 1)

    it "saveUser should fail if user already exists" $ do
      dbConn <- createDbConn
      saveUser dbConn (CreateUserRequest.CreateUserRequest "name" "email" "password") >>= (`shouldBe` Nothing)

    it "getUserByEmail should return" $ do
      dbConn <- createDbConn
      getUserByEmail dbConn "email" >>= (`shouldBe` User 1 "name" "email" "password")

    it "getUserByEmail should fail if user does not exist" $ do
      dbConn <- createDbConn
      getUserByEmail dbConn "doesnotexist" >>= (`shouldBe` UserNotFound)