{-# LANGUAGE OverloadedStrings #-}

module UserSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import qualified Transport.CreateUserRequest as CreateUserRequest
import Model.User

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "UserSpec" $ do

    it "saveUser should save" $ do
      _ <- execute dbConn "TRUNCATE TABLE users" ()
      saveUser dbConn (CreateUserRequest.CreateUserRequest "name" "email" "password") >>= (`shouldBe` Just 1)

    it "saveUser should fail if user already exists" $ do
      saveUser dbConn (CreateUserRequest.CreateUserRequest "name" "email" "password") >>= (`shouldBe` Nothing)

    it "getUserByEmail should return" $ do
      getUserByEmail dbConn "email" >>= (`shouldBe` User 1 "name" "email" "password")

    it "getUserByEmail should fail if user does not exist" $ do
      getUserByEmail dbConn "doesnotexist" >>= (`shouldBe` UserNotFound)

    it "getUserById should return" $ do
      getUserById dbConn 1 >>= (`shouldBe` User 1 "name" "email" "password")

    it "getUserById should fail if user does not exist" $ do
      getUserById dbConn 5 >>= (`shouldBe` UserNotFound)

    it "name should return name" $ do
      name User { userId = 1, name = "name", email = "email", password = "password" } `shouldBe` "name"

    it "email should return email" $ do
      email User { userId = 1, name = "name", email = "email", password = "password" } `shouldBe` "email"

    it "password should return password" $ do
      password User { userId = 1, name = "name", email = "email", password = "password" } `shouldBe` "password"

    it "show user should show" $ do
      show User { userId = 1, name = "name", email = "email", password = "password" } `shouldBe` "User {userId = 1, name = \"name\", email = \"email\", password = \"password\"}"