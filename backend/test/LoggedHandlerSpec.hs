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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.IO.Class
import Network.Wai.Test
import Network.HTTP.Types
import App
import qualified Data.String as BS
import qualified Data.Text.Encoding as BSL

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

  with (api "avm_test") $ do
    describe "SignUpHandlerSpec" $ do

      it "SignUp return invalid Token" $ do
        liftIO (cleanDb dbConn)
        post "/loggedHandler" "data" `shouldRespondWith` "Invalid Token"

      it "SignUp return Token" $ do
        liftIO (cleanDb dbConn)
        post "/signup" createUserRequest `shouldRespondWith` "User created, id: 1"
        response <- post "/signin" signInRequest
        let token = simpleBody response
        a <- (Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest)
        liftIO (print a)
        Test.Hspec.Wai.request methodPost "/loggedHandler" [("Authorization", "Bearer " <> BSL.toStrict token)] loggerHandlerRequest `shouldRespondWithPredicate` (\response -> "Session" `TL.isPrefixOf` response, "Start of the token does not match expected")