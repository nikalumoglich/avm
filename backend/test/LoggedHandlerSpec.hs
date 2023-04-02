{-# LANGUAGE OverloadedStrings #-}

module LoggedHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Database.MySQL.Simple
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.IO.Class
import Network.Wai.Test
import Network.HTTP.Types
import App
import Control.Monad

shouldRespondWithPredicate :: Control.Monad.IO.Class.MonadIO m => m SResponse -> (TL.Text -> Bool, String) -> m ()
shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) []
    else mapM_ (liftIO . expectationFailure) [errorMessage]


cleanDb :: Connection -> IO ()
cleanDb dbConn =
  execute dbConn "TRUNCATE TABLE users" ()
  >> execute dbConn "TRUNCATE TABLE permissions" ()
  >> execute dbConn "TRUNCATE TABLE users_permissions" ()
  >> execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel');" ()
  >> execute dbConn "INSERT INTO permissions (permission) VALUES ('adminLevel')" ()
  >> return ()

createUserRequest :: DBL.ByteString
createUserRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

signInRequest :: DBL.ByteString
signInRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

loggerHandlerRequest :: DBL.ByteString
loggerHandlerRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

suiteSpec :: Connection -> Spec
suiteSpec dbConn = do

  with (api "127.0.0.1" "avm_test" "haskelluser" "haskellpassword" "secret2") $ do
    describe "SignUpHandlerSpec" $ do

      it "LoggedHandler return invalid Token" $ do
        liftIO (cleanDb dbConn)
        post "/loggedHandler" "data" `shouldRespondWith` "Invalid Token"

      it "LoggedHandler should fail if user does not have permission" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "User created, id: 1"
        response <- post "/signin" signInRequest
        let token = simpleBody response
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest `shouldRespondWith` "Invalid Token"

      it "LoggedHandler return success" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "User created, id: 1"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let token = simpleBody response
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest `shouldRespondWithPredicate` (\response -> "Session" `TL.isPrefixOf` response, "Start of the token does not match expected")

      it "LoggedHandler should fail with invalid json" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "User created, id: 1"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let token = simpleBody response
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] "invalid data" `shouldRespondWith` "Invalid JSON"
