{-# LANGUAGE OverloadedStrings #-}

module SignUpHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString.Lazy as DBL
import App

validRequest :: DBL.ByteString
validRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

suiteSpec :: Connection -> Spec
suiteSpec dbConn = do

  with (api "127.0.0.1" "avm_test" "haskelluser" "haskellpassword" "secret2") $ do
    describe "SignUpHandlerSpec" $ do

      it "SignUp return invalid JSON" $ do
        post "/signup" "data" `shouldRespondWith` "Invalid JSON"

      it "SignUp should create user" $ do
        _ <- liftIO (execute dbConn "TRUNCATE TABLE users" ())
        post "/signup" validRequest `shouldRespondWith` "User created, id: 1"

      it "SignUp should not create user again" $ do
        post "/signup" validRequest `shouldRespondWith` "User not created"