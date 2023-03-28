{-# LANGUAGE OverloadedStrings #-}

module SessionSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Database.MySQL.Simple
import qualified Model.User as User
import Data.Time.Clock.POSIX
import Model.Session

createDbConn :: IO Connection
createDbConn = connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm_test" })

suiteSpec :: Spec
suiteSpec = do
  describe "SessionSpec" $ do

    it "saveSession should save" $ do
      dbConn <- createDbConn
      _ <- execute dbConn "TRUNCATE TABLE sessions" ()
      currentTimestamp <- getPOSIXTime
      saveSession dbConn (User.User 1 "name" "email" "password") >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session" $ do
      dbConn <- createDbConn
      currentTimestamp <- getPOSIXTime
      getActiveSession dbConn (User.User 1 "name" "email" "password") >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session not found" $ do
      dbConn <- createDbConn
      getActiveSession dbConn (User.User 2 "name" "email" "password") >>= (`shouldBe` SessionNotFound)

    it "renewSession should return session with new expiration date" $ do
      dbConn <- createDbConn
      currentTimestamp <- getPOSIXTime
      renewSession dbConn (Session { sessionId = 1, userId = 1, expiration = 0 }) >>= (`shouldBe` Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "renewSession should return session not found" $ do
      dbConn <- createDbConn
      renewSession dbConn (Session { sessionId = 2, userId = 1, expiration = 0 }) >>= (`shouldBe` SessionNotFound)

    it "show session should show" $ do
      show Session { sessionId = 1, userId = 2, expiration = 3 } `shouldBe` "Session {sessionId = 1, userId = 2, expiration = 3}"