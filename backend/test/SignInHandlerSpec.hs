{-# LANGUAGE OverloadedStrings #-}

module SignInHandlerSpec
    ( suiteSpec
    ) where

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai.Test
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.ByteString.Lazy as DBL
import Control.Monad.IO.Class
import App

validLoginRequest :: DBL.ByteString
validLoginRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepassword\" \
\}"

invalidLoginRequest :: DBL.ByteString
invalidLoginRequest = "{ \
\    \"email\": \"email@example.com\", \
\    \"password\": \"somepasswordwrong\" \
\}"

invalidLoginRequest2 :: DBL.ByteString
invalidLoginRequest2 = "{ \
\    \"email\": \"wrong email\", \
\    \"password\": \"somepasswordwrong\" \
\}"

shouldRespondWithPredicate :: Control.Monad.IO.Class.MonadIO m => m SResponse -> (TL.Text -> Bool, String) -> m ()
shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) []
    else mapM_ (liftIO . expectationFailure) [errorMessage]

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
        post "/signin" validLoginRequest `shouldRespondWithPredicate` (f, "Start of the token does not match expected") where f response = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" `TL.isPrefixOf` response


