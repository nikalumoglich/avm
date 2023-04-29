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

  with (api "127.0.0.1" "avm_test" "haskelluser" "haskellpassword" "secret2" 60) $ do
    describe "SignUpHandlerSpec" $ do

      it "SignUp return invalid JSON" $ do
        post "/signup" "data" `shouldRespondWith` "{\"code\":1,\"message\":\"Invalid Json format\"}" { matchStatus = 400 }

      it "SignUp should create user" $ do
        _ <- liftIO (execute dbConn "TRUNCATE TABLE users" ())
        post "/signup" validRequest `shouldRespondWith` "{\"id\":1}"

      it "SignUp should not create user again" $ do
        post "/signup" validRequest `shouldRespondWith` "{\"code\":5,\"message\":\"User already exist\"}" { matchStatus = 400 }