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

-- shouldRespondWithPredicate :: HasCallStack => WaiSession st SResponse -> String -> (TL.Text -> Bool) -> Bool -> WaiExpectation st
shouldRespondWithPredicate action (matcher, errorMessage) = do
  r <- action
  liftIO $ print (simpleBody r)
  if matcher (EL.decodeUtf8  (simpleBody r))
    then mapM_ (liftIO . expectationFailure) [] 
    else mapM_ (liftIO . expectationFailure) [(errorMessage)]

match' :: SResponse -> ResponseMatcher -> Maybe String
match' (SResponse (Status status _) headers body) (ResponseMatcher expectedStatus expectedHeaders (MatchBody bodyMatcher)) = mconcat [
    actualExpected "status mismatch:" (show status) (show expectedStatus) <$ guard (status /= expectedStatus)
  , checkHeaders headers body expectedHeaders
  , bodyMatcher headers body
  ]

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]

checkHeaders :: [Header] -> Body -> [MatchHeader] -> Maybe String
checkHeaders headers body m = case go m of
    [] -> Nothing
    xs -> Just (mconcat xs ++ "the actual headers were:\n" ++ unlines (map formatHeader headers))
  where
    go = catMaybes . map (\(MatchHeader p) -> p headers body)


suiteSpec :: Spec
suiteSpec = do

  with (api "avm_test") $ do
    describe "SignInHandlerSpec" $ do

      it "SignIn return invalid JSON" $ do
        post "/signin" "data" `shouldRespondWith` "Invalid JSON"

      it "SignIn should not login user" $ do
        post "/signin" invalidLoginRequest `shouldRespondWith` "Not OK"

      it "SignIn should find user" $ do
        post "/signin" invalidLoginRequest2 `shouldRespondWith` "Not OK"

      it "SignIn should login user" $ do
        post "/signin" validLoginRequest `shouldRespondWithPredicate` (f,"Start of the token does not match expected") where f = (\response -> "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" `TL.isPrefixOf` response)
        

      