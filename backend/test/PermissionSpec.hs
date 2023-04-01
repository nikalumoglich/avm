{-# LANGUAGE OverloadedStrings #-}

module PermissionSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import qualified Model.User as User
import qualified Transport.CreateUserRequest as CreateUserRequest
import Control.Monad
import Data.Maybe
import Model.Permission


cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE users" ()
  >> execute dbConn "TRUNCATE TABLE permissions" ()
  >> execute dbConn "TRUNCATE TABLE users_permissions" ()
  >> execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel');" ()
  >> execute dbConn "INSERT INTO permissions (permission) VALUES ('adminLevel')" ()
  >> return ()

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "SessionSpec" $ do

    it "should get correct permissions" $ do
      cleanDb dbConn
      maybeUserId <- User.saveUser dbConn CreateUserRequest.CreateUserRequest { CreateUserRequest.password = "password", CreateUserRequest.name = "name", CreateUserRequest.email = "email" }
      let userId = fromJust maybeUserId
      void (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (userId, 1 :: Int))
      permissions <- getUserPermissions dbConn userId
      permissions `shouldBe` [Permission { permission = "userLevel" }]
      head permissions `shouldBe` Permission { permission = "userLevel" }

    it "should show permissions" $ do
      cleanDb dbConn
      maybeUserId <- User.saveUser dbConn CreateUserRequest.CreateUserRequest { CreateUserRequest.password = "password", CreateUserRequest.name = "name", CreateUserRequest.email = "email" }
      let userId = fromJust maybeUserId
      void (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (userId, 1 :: Int))
      permissions <- getUserPermissions dbConn userId
      show permissions `shouldBe` "[Permission {permission = \"userLevel\"}]"

    it "should get permission string" $ do
      cleanDb dbConn
      maybeUserId <- User.saveUser dbConn CreateUserRequest.CreateUserRequest { CreateUserRequest.password = "password", CreateUserRequest.name = "name", CreateUserRequest.email = "email" }
      let userId = fromJust maybeUserId
      void (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (userId, 1 :: Int))
      permissions <- getUserPermissions dbConn userId
      permission (head permissions) `shouldBe` "userLevel"

    it "should compare permissions" $ do
      cleanDb dbConn
      maybeUserId <- User.saveUser dbConn CreateUserRequest.CreateUserRequest { CreateUserRequest.password = "password", CreateUserRequest.name = "name", CreateUserRequest.email = "email" }
      let userId = fromJust maybeUserId
      void (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (userId, 1 :: Int))
      permissions <- getUserPermissions dbConn userId
      head permissions == Permission { permission = "userLevel" } `shouldBe` True

{-
    it "renewSession should return session not found" $ do
      renewSession dbConn (Session { sessionId = 2, userId = 1, expiration = 0 }) >>= (`shouldBe` SessionNotFound)

    it "show session should show" $ do
      show Session { sessionId = 1, userId = 2, expiration = 3 } `shouldBe` "Session {sessionId = 1, userId = 2, expiration = 3}"

    it "getActiveSession should return session not found for expired session" $ do
      _ <- execute dbConn "UPDATE sessions SET expiration = 0 where 1 = 1" ()
      getActiveSession dbConn 1 >>= (`shouldBe` SessionNotFound)
      -}