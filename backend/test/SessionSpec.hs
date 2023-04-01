{-# LANGUAGE OverloadedStrings #-}

module SessionSpec
    ( suiteSpec
    ) where

import qualified Test.HUnit.Lang as HunitLang
import Data.CallStack
import Control.Exception as E

import Test.Hspec
import Database.MySQL.Simple
import qualified Model.User as User
import Data.Time.Clock.POSIX
import Model.Session

shouldBeWithValidationFunction :: (a -> a -> IO Bool) -> String -> a -> a -> HunitLang.Assertion
shouldBeWithValidationFunction f errorMessage actual expected = do
  comparisonResult <- f actual expected
  if comparisonResult then return ()
  else E.throwIO (HunitLang.HUnitFailure location $ HunitLang.ExpectedButGot (Just "") errorMessage "False")

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

suiteSpec :: Connection -> SpecWith ()
suiteSpec dbConn = do
  describe "SessionSpec" $ do

    it "saveSession should save" $ do
      _ <- execute dbConn "TRUNCATE TABLE sessions" ()
      currentTimestamp <- getPOSIXTime
      shouldBeWithValidationFunction (\actual expected -> do
                                                actualValue <- actual
                                                expectedValue <- expected
                                                return (abs (expiration actualValue - expiration expectedValue) <= 5)
        ) "Sessions with expiration date within a 11 seconds interval" (saveSession dbConn (User.User 1 "name" "email" "password")) (return Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session" $ do
      currentTimestamp <- getPOSIXTime
      shouldBeWithValidationFunction (\actual expected -> do
                                                actualValue <- actual
                                                expectedValue <- expected
                                                return (abs (expiration actualValue - (expiration expectedValue)) <= 5)
        ) "Sessions with expiration date within a 11 seconds interval" (getActiveSession dbConn 1) (return Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "getActiveSession should return session not found" $ do
      getActiveSession dbConn 2 >>= (`shouldBe` SessionNotFound)

    it "renewSession should return session with new expiration date" $ do
      currentTimestamp <- getPOSIXTime
      shouldBeWithValidationFunction (\actual expected -> do
                                                actualValue <- actual
                                                expectedValue <- expected
                                                return (abs ((expiration actualValue) - (expiration expectedValue)) <= 5)
        ) "Sessions with expiration date within a 11 seconds interval" (renewSession dbConn (Session { sessionId = 1, userId = 1, expiration = 0 })) (return Session { sessionId = 1, userId = 1, expiration = round currentTimestamp + 60 })

    it "renewSession should return session not found" $ do
      renewSession dbConn (Session { sessionId = 2, userId = 1, expiration = 0 }) >>= (`shouldBe` SessionNotFound)

    it "show session should show" $ do
      show Session { sessionId = 1, userId = 2, expiration = 3 } `shouldBe` "Session {sessionId = 1, userId = 2, expiration = 3}"

    it "getActiveSession should return session not found for expired session" $ do
      _ <- execute dbConn "UPDATE sessions SET expiration = 0 where 1 = 1" ()
      getActiveSession dbConn 1 >>= (`shouldBe` SessionNotFound)