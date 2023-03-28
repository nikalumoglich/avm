{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SignUpHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Database.MySQL.Simple
import qualified Transport.CreateUserRequest as CreateUserRequest
import qualified Model.User as User
import Handlers.SignUpHandler
import Network.Wai (Application)
import App

createDbConn :: IO Connection
createDbConn = connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm_test" })

validRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"tiozao@tiozao.com\", \
\    \"password\": \"somepassword\" \
\}"

suiteSpec :: Spec
suiteSpec = do

  with (api "avm_test") $ do
    describe "SignUpHandlerSpec" $ do

      it "SignUp return invalid JSON" $ do
        post "/signup" "data" `shouldRespondWith` "Invalid JSON"

      it "SignUp should create user" $ do
        dbConn <- liftIO (createDbConn)
        _ <- liftIO (execute dbConn "TRUNCATE TABLE users" ())
        post "/signup" validRequest `shouldRespondWith` "User created, id: 1"

      it "SignUp should not create user again" $ do
        post "/signup" validRequest `shouldRespondWith` "User not created"