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
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Control.Monad.IO.Class
import Network.Wai.Test
import Network.HTTP.Types
import App
import Control.Monad
import Data.Maybe
import qualified Data.Aeson as Aeson
import qualified Security.Jwt as Jwt

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
  >> execute dbConn "INSERT INTO permissions (permission) VALUES ('userLevel')" ()
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

suiteSpec :: Connection -> String -> String -> String -> String -> String -> Spec
suiteSpec dbConn host database user password bucket = do

  with (api host database user password "secret2" 60 bucket) $ do
    describe "LoggedHandlerSpec" $ do

      it "LoggedHandler return invalid Token" $ do
        liftIO (cleanDb dbConn)
        post "/loggedHandler" "data" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail with incorrect token" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        _ <- post "/signin" signInRequest
        let token = "incorrect token"
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail if session is not found" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        liftIO (execute dbConn "TRUNCATE TABLE sessions" ())
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail if Json data is invalid" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] "Invalid data" `shouldRespondWith` "{\"code\":1,\"message\":\"Invalid Json format\"}" { matchStatus = 400 }

      it "LoggedHandler should return success" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWithPredicate` (\response -> "Session" `TL.isPrefixOf` response, "Start of the token does not match expected")

      it "LoggedHandler should fail because user does not have permission" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        response <- post "/signin" signInRequest
        let rawToken = simpleBody response
        let token = fromJust (Aeson.decode rawToken)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict (BSLU.fromString (Jwt.token token)))] loggerHandlerRequest `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }

      it "LoggedHandler should fail with invalid json" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "{\"id\":1}"
        void (liftIO (execute dbConn "INSERT INTO users_permissions (user_id, permission_id) VALUES (?, ?)" (1 :: Int, 1 :: Int)))
        response <- post "/signin" signInRequest
        let token = simpleBody response
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] "invalid data" `shouldRespondWith` "{\"code\":2,\"message\":\"Invalid Session\"}" { matchStatus = 401 }
