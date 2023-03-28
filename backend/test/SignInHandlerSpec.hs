{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SignInHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Database.MySQL.Simple
import qualified Transport.CreateUserRequest as CreateUserRequest
import qualified Model.User as User
import Handlers.SignInHandler
import Network.Wai (Application)
import Network.Wai.Test
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Encoding as E
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as ByteString
import App
import Test.Hspec.Wai.Matcher



import Test.Hspec.Wai.Matcher
import           Network.HTTP.Types

createDbConn :: IO Connection
createDbConn = connect (defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "haskelluser", connectPassword = "haskellpassword", connectDatabase = "avm_test" })

validCreateUserRequest = "{ \
\    \"name\": \"Andre\", \
\    \"email\": \"tiozao@tiozao.com\", \
\    \"password\": \"somepassword\" \
\}"

validLoginRequest = "{ \
\    \"email\": \"tiozao@tiozao.com\", \
\    \"password\": \"somepassword\" \
\}"

invalidLoginRequest = "{ \
\    \"email\": \"tiozao@tiozao.com\", \
\    \"password\": \"somepasswordwrong\" \
\}"

invalidLoginRequest2 = "{ \
\    \"email\": \"wrong email\", \
\    \"password\": \"somepasswordwrong\" \
\}"

shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) [] 
    else mapM_ (liftIO . expectationFailure) [(errorMessage)]

suiteSpec :: Spec
suiteSpec = do

  with (api "avm_test") $ do
    describe "SignInHandlerSpec" $ do

      it "SignIn return invalid JSON" $ do
        post "/signin" "data" `shouldRespondWith` "Invalid JSON"

      it "SignIn should not login user" $ do
        post "/signin" invalidLoginRequest `shouldRespondWith` "Not OK"

      it "SignIn should not find user" $ do
        post "/signin" invalidLoginRequest2 `shouldRespondWith` "User not found"

      it "SignIn should login user" $ do
        post "/signin" validLoginRequest `shouldRespondWithPredicate` (f,"Start of the token does not match expected") where f = (\response -> "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" `TL.isPrefixOf` response)
        

      