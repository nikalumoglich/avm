{-# LANGUAGE OverloadedStrings #-}

module SessionSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import qualified Model.User as User
import Data.Time.Clock.POSIX
import Model.Session

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "SessionSpec" $ do

    it "saveSession should save" $ do
      _ <- execute dbConn "TRUNCATE TABLE sessions" ()
      currentTimestamp <- getPOSIXTime
      saveSession dbConn (User.User 1 "name" "email" "password") >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session" $ do
      currentTimestamp <- getPOSIXTime
      getActiveSession dbConn 1 >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session not found" $ do
      getActiveSession dbConn 2 >>= (`shouldBe` SessionNotFound)

    it "renewSession should return session with new expiration date" $ do
      currentTimestamp <- getPOSIXTime
      renewSession dbConn (Session { sessionId = 1, userId = 1, expiration = 0 }) >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "renewSession should return session not found" $ do
      renewSession dbConn (Session { sessionId = 2, userId = 1, expiration = 0 }) >>= (`shouldBe` SessionNotFound)

    it "show session should show" $ do
      show Session { sessionId = 1, userId = 2, expiration = 3 } `shouldBe` "Session {sessionId = 1, userId = 2, expiration = 3}"

    it "getActiveSession should return session not found for expired session" $ do
      _ <- execute dbConn "UPDATE sessions SET expiration = 0 where 1 = 1" ()
      getActiveSession dbConn 1 >>= (`shouldBe` SessionNotFound)